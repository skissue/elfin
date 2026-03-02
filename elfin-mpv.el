;;; elfin-mpv.el --- mpv integration -*- lexical-binding: t -*-

;;; Commentary:

;; mpv process management, IPC, and event system.

;;; Code:

(require 'cl-lib)

(defconst elfin--mpv-socket "/tmp/elfin-mpv.sock"
  "Path to mpv IPC socket.")

(defvar elfin--mpv-process nil
  "Current mpv process for audio playback.")

(defvar elfin--property-observers nil
  "Alist mapping observer IDs to property names ((ID . property) ...).")

(defvar elfin--observer-counter 0
  "Counter for generating unique observer IDs.")

(defvar elfin--property-callbacks nil
  "Alist of (property . (list of fns)) for property change callbacks.")

(defvar elfin--event-callbacks nil
  "Alist of (event-name . (list of fns)) for event callbacks.")

(defvar elfin--ipc-process nil
  "Persistent IPC connection to mpv.")

(defvar elfin--ipc-buffer ""
  "Buffer for accumulating incomplete IPC output.")

(defvar elfin--pending-response nil
  "Storage for synchronous command responses.")

(defun elfin--ipc-log (direction string)
  "When `elfin-debug' is non-nil, log STRING with DIRECTION tag to *Elfin IPC Log*."
  (when elfin-debug
    (with-current-buffer (get-buffer-create "*Elfin IPC Log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "[%T] ")
                direction ": " string)))))

(defun elfin--normalize-value (value)
  "Normalize mpv VALUE: :null and :false to nil, :true to t."
  (pcase value
    ((or :null :false) nil)
    (:true t)
    (_ value)))

(defun elfin--ipc-filter (_proc output)
  "Process OUTPUT from mpv IPC, parsing JSON lines and dispatching events."
  (elfin--ipc-log "IN" output)
  (setq elfin--ipc-buffer (concat elfin--ipc-buffer output))
  (let ((lines (split-string elfin--ipc-buffer "\n" t)))
    (if (string-suffix-p "\n" output)
        (setq elfin--ipc-buffer "")
      (setq elfin--ipc-buffer (car (last lines)))
      (setq lines (butlast lines)))
    (let (property-changes events)
      (cl-loop for line in lines
               for json = (ignore-errors (json-parse-string line))
               when json do
               (let ((event-name (gethash "event" json)))
                 (cond
                  ((not event-name)
                   (setq elfin--pending-response json))
                  ((string= event-name "property-change")
                   (push json property-changes))
                  (t
                   (push json events)))))
      (cl-loop for change in (nreverse property-changes)
               for id = (gethash "id" change)
               for entry = (and id (assq id elfin--property-observers))
               for property = (and entry (cdr entry))
               for cb-entry = (and property (assoc property elfin--property-callbacks))
               when cb-entry do
               (let ((value (elfin--normalize-value (gethash "data" change))))
                 (dolist (fn (cdr cb-entry))
                   (funcall fn value))))
      (cl-loop for event in (nreverse events)
               for event-name = (gethash "event" event)
               for cb-entry = (assoc event-name elfin--event-callbacks)
               when cb-entry do
               (dolist (fn (cdr cb-entry))
                 (funcall fn event))))))

(defun elfin--reregister-observers ()
  "Re-register all property observers after reconnect."
  (dolist (entry elfin--property-observers)
    (elfin--mpv-send "observe_property" (car entry) (cdr entry))))

(defun elfin--mpv-command ()
  "Format and return command to start mpv process."
  (list "mpv" "--no-video" "--idle"
        "--quiet" "--msg-color=no" "--term-osd=no"
        (format "--input-ipc-server=%s" elfin--mpv-socket)
        (format "--volume=%d" elfin-default-volume)))

(defun elfin--ensure-mpv ()
  "Ensure mpv is running.
Return non-nil if there was an error."
  (unless (and elfin--mpv-process
               (process-live-p elfin--mpv-process))
    (when (file-exists-p elfin--mpv-socket)
      (delete-file elfin--mpv-socket))
    (setq elfin--mpv-process
          (make-process :name "elfin-mpv"
                        :buffer (get-buffer-create "*Elfin mpv*")
                        :command (elfin--mpv-command)
                        :sentinel (lambda (_proc event)
                                    (message "mpv: %s" (string-trim event)))))
    (let ((tries 50))
      (while (and (> tries 0)
                  (not (file-exists-p elfin--mpv-socket)))
        (sleep-for 0.05)
        (cl-decf tries))
      (unless (file-exists-p elfin--mpv-socket)
        (error "Failed to start mpv")))))

(defun elfin--mpv-send (&rest args)
  "Send ARGS to mpv via persistent IPC connection.
Returns the parsed JSON response synchronously."
  (elfin--ensure-ipc)
  (setq elfin--pending-response nil)
  (let ((json (concat (json-serialize `(:command ,(apply #'vector args))) "\n")))
    (elfin--ipc-log "OUT" json)
    (process-send-string elfin--ipc-process json)
    (let ((tries 100))
      (while (and (> tries 0) (null elfin--pending-response))
        (accept-process-output elfin--ipc-process 0.01)
        (cl-decf tries)))
    elfin--pending-response))

(defun elfin--ensure-ipc ()
  "Ensure persistent IPC connection to mpv exists."
  (unless (and elfin--ipc-process
               (process-live-p elfin--ipc-process))
    (elfin--ensure-mpv)
    (setq elfin--ipc-buffer "")
    (setq elfin--ipc-process
          (make-network-process
           :name "elfin-ipc"
           :buffer nil
           :family 'local
           :service elfin--mpv-socket
           :filter #'elfin--ipc-filter
           :sentinel (lambda (_proc _event)
                       (setq elfin--ipc-process nil))))
    (elfin--reregister-observers)))

(defun elfin-observe-property (property fn)
  "Observe PROPERTY changes, calling FN with the new value.
FN receives the new property value as its single argument.
Values are normalized: :null and :false become nil, :true becomes t.
If PROPERTY is already being observed, FN is simply added to the callback list."
  (elfin--ensure-ipc)
  (unless (assoc property elfin--property-callbacks)
    (push (list property) elfin--property-callbacks)
    (let ((id (cl-incf elfin--observer-counter)))
      (push (cons id property) elfin--property-observers)
      (elfin--mpv-send "observe_property" id property)))
  (cl-pushnew fn (cdr (assoc property elfin--property-callbacks))))

(defun elfin-unobserve-property (property fn)
  "Remove FN from observers of PROPERTY.
If no observers remain, unregister the property from mpv."
  (when-let* ((cb-entry (assoc property elfin--property-callbacks)))
    (cl-callf2 delq fn (cdr cb-entry))
    (unless (cdr cb-entry)
      (cl-callf2 assoc-delete-all property elfin--property-callbacks)
      (when-let* ((obs (rassoc property elfin--property-observers)))
        (elfin--mpv-send "unobserve_property" (car obs))
        (cl-callf2 delq obs elfin--property-observers)))))

(defun elfin--add-event-handler (event-name fn)
  "Add FN as a handler for EVENT-NAME events from mpv.
FN receives the full event hash table as its single argument."
  (unless (assoc event-name elfin--event-callbacks)
    (push (list event-name) elfin--event-callbacks))
  (cl-pushnew fn (cdr (assoc event-name elfin--event-callbacks))))

(defun elfin-kill ()
  "Kill the mpv process."
  (interactive)
  (when (and elfin--mpv-process
             (process-live-p elfin--mpv-process))
    (delete-process elfin--mpv-process)
    (setq elfin--mpv-process nil)
    (when (file-exists-p elfin--mpv-socket)
      (delete-file elfin--mpv-socket))
    (message "Killed mpv process")))

(provide 'elfin-mpv)

;;; elfin-mpv.el ends here
