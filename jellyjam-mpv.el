;;; jellyjam-mpv.el --- mpv integration -*- lexical-binding: t -*-

;;; Commentary:

;; mpv process management, IPC, and event system.

;;; Code:

(require 'cl-lib)

(defconst jellyjam--mpv-socket "/tmp/jellyjam-mpv.sock"
  "Path to mpv IPC socket.")

(defvar jellyjam--mpv-process nil
  "Current mpv process for audio playback.")

(defvar jellyjam--property-observers nil
  "Alist mapping observer IDs to property names ((ID . property) ...).")

(defvar jellyjam--observer-counter 0
  "Counter for generating unique observer IDs.")

(defvar jellyjam--property-callbacks nil
  "Alist of (property . (list of fns)) for property change callbacks.")

(defvar jellyjam--event-callbacks nil
  "Alist of (event-name . (list of fns)) for event callbacks.")

(defvar jellyjam--ipc-process nil
  "Persistent IPC connection to mpv.")

(defvar jellyjam--ipc-buffer ""
  "Buffer for accumulating incomplete IPC output.")

(defvar jellyjam--pending-response nil
  "Storage for synchronous command responses.")

(defun jellyjam--normalize-value (value)
  "Normalize mpv VALUE: :null and :false to nil, :true to t."
  (pcase value
    ((or :null :false) nil)
    (:true t)
    (_ value)))

(defun jellyjam--ipc-filter (_proc output)
  "Process OUTPUT from mpv IPC, parsing JSON lines and dispatching events."
  (setq jellyjam--ipc-buffer (concat jellyjam--ipc-buffer output))
  (let ((lines (split-string jellyjam--ipc-buffer "\n" t)))
    (if (string-suffix-p "\n" output)
        (setq jellyjam--ipc-buffer "")
      (setq jellyjam--ipc-buffer (car (last lines)))
      (setq lines (butlast lines)))
    (let (property-changes events)
      (cl-loop for line in lines
               for json = (ignore-errors (json-parse-string line))
               when json do
               (let ((event-name (gethash "event" json)))
                 (cond
                  ((not event-name)
                   (setq jellyjam--pending-response json))
                  ((string= event-name "property-change")
                   (push json property-changes))
                  (t
                   (push json events)))))
      (cl-loop for change in (nreverse property-changes)
               for id = (gethash "id" change)
               for entry = (and id (assq id jellyjam--property-observers))
               for property = (and entry (cdr entry))
               for cb-entry = (and property (assoc property jellyjam--property-callbacks))
               when cb-entry do
               (let ((value (jellyjam--normalize-value (gethash "data" change))))
                 (dolist (fn (cdr cb-entry))
                   (funcall fn value))))
      (cl-loop for event in (nreverse events)
               for event-name = (gethash "event" event)
               for cb-entry = (assoc event-name jellyjam--event-callbacks)
               when cb-entry do
               (dolist (fn (cdr cb-entry))
                 (funcall fn event))))))

(defun jellyjam--reregister-observers ()
  "Re-register all property observers after reconnect."
  (dolist (entry jellyjam--property-observers)
    (jellyjam--mpv-send "observe_property" (car entry) (cdr entry))))

(defun jellyjam--mpv-command ()
  "Format and return command to start mpv process."
  (list "mpv" "--no-video" "--idle"
        "--quiet" "--msg-color=no" "--term-osd=no"
        (format "--input-ipc-server=%s" jellyjam--mpv-socket)
        (format "--volume=%d" jellyjam-default-volume)))

(defun jellyjam--ensure-mpv ()
  "Ensure mpv is running.
Return non-nil if there was an error."
  (unless (and jellyjam--mpv-process
               (process-live-p jellyjam--mpv-process))
    (when (file-exists-p jellyjam--mpv-socket)
      (delete-file jellyjam--mpv-socket))
    (setq jellyjam--mpv-process
          (make-process :name "jellyjam-mpv"
                        :buffer (get-buffer-create "*Jellyjam mpv*")
                        :command (jellyjam--mpv-command)
                        :sentinel (lambda (_proc event)
                                    (message "mpv: %s" (string-trim event)))))
    (let ((tries 50))
      (while (and (> tries 0)
                  (not (file-exists-p jellyjam--mpv-socket)))
        (sleep-for 0.05)
        (cl-decf tries))
      (unless (file-exists-p jellyjam--mpv-socket)
        (error "Failed to start mpv")))))

(defun jellyjam--mpv-send (&rest args)
  "Send ARGS to mpv via persistent IPC connection.
Returns the parsed JSON response synchronously."
  (jellyjam--ensure-ipc)
  (setq jellyjam--pending-response nil)
  (let ((json (concat (json-serialize `(:command ,(apply #'vector args))) "\n")))
    (process-send-string jellyjam--ipc-process json)
    (let ((tries 100))
      (while (and (> tries 0) (null jellyjam--pending-response))
        (accept-process-output jellyjam--ipc-process 0.01)
        (cl-decf tries)))
    jellyjam--pending-response))

(defun jellyjam--ensure-ipc ()
  "Ensure persistent IPC connection to mpv exists."
  (unless (and jellyjam--ipc-process
               (process-live-p jellyjam--ipc-process))
    (jellyjam--ensure-mpv)
    (setq jellyjam--ipc-buffer "")
    (setq jellyjam--ipc-process
          (make-network-process
           :name "jellyjam-ipc"
           :buffer nil
           :family 'local
           :service jellyjam--mpv-socket
           :filter #'jellyjam--ipc-filter
           :sentinel (lambda (_proc _event)
                       (setq jellyjam--ipc-process nil))))
    (jellyjam--reregister-observers)))

(defun jellyjam-observe-property (property fn)
  "Observe PROPERTY changes, calling FN with the new value.
FN receives the new property value as its single argument.
Values are normalized: :null and :false become nil, :true becomes t.
If PROPERTY is already being observed, FN is simply added to the callback list."
  (jellyjam--ensure-ipc)
  (unless (assoc property jellyjam--property-callbacks)
    (push (list property) jellyjam--property-callbacks)
    (let ((id (cl-incf jellyjam--observer-counter)))
      (push (cons id property) jellyjam--property-observers)
      (jellyjam--mpv-send "observe_property" id property)))
  (cl-pushnew fn (cdr (assoc property jellyjam--property-callbacks))))

(defun jellyjam--add-event-handler (event-name fn)
  "Add FN as a handler for EVENT-NAME events from mpv.
FN receives the full event hash table as its single argument."
  (unless (assoc event-name jellyjam--event-callbacks)
    (push (list event-name) jellyjam--event-callbacks))
  (cl-pushnew fn (cdr (assoc event-name jellyjam--event-callbacks))))

(defun jellyjam-kill ()
  "Kill the mpv process."
  (interactive)
  (when (and jellyjam--mpv-process
             (process-live-p jellyjam--mpv-process))
    (delete-process jellyjam--mpv-process)
    (setq jellyjam--mpv-process nil)
    (when (file-exists-p jellyjam--mpv-socket)
      (delete-file jellyjam--mpv-socket))
    (message "Killed mpv process")))

(provide 'jellyjam-mpv)

;;; jellyjam-mpv.el ends here
