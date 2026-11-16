;;; elfin-test-helper.el --- Offline test fixtures -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'elfin)
(require 'elfin-listenbrainz)

(defmacro elfin-test-with-http (&rest body)
  "Run BODY with HTTP calls captured in `requests', newest first.
Callbacks remain pending until the test explicitly delivers a response."
  (declare (indent 0) (debug t))
  `(let ((elfin--active-session '(:server "https://jellyfin.invalid"
                                :access-token "test-token" :user-id "user-1"))
         (elfin--sessions nil)
         (elfin-session-file nil)
         (elfin-debug nil)
         requests)
     (cl-letf (((symbol-function 'plz)
                (lambda (method url &rest options)
                  (push (list method url options) requests)
                  nil)))
       ,@body)))

(defun elfin-test-respond (request json &optional failure)
  "Deliver JSON to captured REQUEST, or FAILURE to its error callback."
  (let ((options (nth 2 request)))
    (funcall (plist-get options (if failure :else :then))
             (cond (failure failure)
                   ((eq (plist-get options :as) 'string) json)
                   (t (json-parse-string json))))))

(defun elfin-test-wait-for (predicate)
  "Wait for PREDICATE to become non-nil, failing after two seconds."
  (let ((deadline (time-add (current-time) 2)) result)
    (while (and (not (setq result (funcall predicate)))
                (time-less-p (current-time) deadline))
      (accept-process-output nil 0.01))
    (should result)
    result))

(defun elfin-test-await (future)
  "Await FUTURE within the active mocks, failing after two seconds."
  (should (futur-p future))
  (let (done value failure)
    (futur-bind future
                (lambda (result) (setq value result done t) nil)
                (lambda (err) (setq failure err done t) nil))
    (elfin-test-wait-for (lambda () done))
    (if failure (signal (car failure) (cdr failure)) value)))

(defun elfin-test-query (request)
  "Return decoded query parameters of captured REQUEST as an alist."
  (mapcar (lambda (pair)
            (mapcar #'url-unhex-string (split-string pair "=")))
          (split-string (cadr (split-string (nth 1 request) "?")) "&" t)))

(defmacro elfin-test-with-player (&rest body)
  "Run BODY with isolated playback state and captured mpv `commands'."
  (declare (indent 0) (debug t))
  `(let ((elfin--queue nil)
         (elfin--queue-pos nil)
         (elfin-current-track-id nil)
         (elfin--entry-map (make-hash-table :test #'eql))
         (elfin-file-start-hook nil)
         (elfin-file-end-hook nil)
         (elfin-pause-hook nil)
         (elfin-idle-hook nil)
         commands)
     (cl-letf (((symbol-function 'elfin--mpv-send)
                (lambda (args &optional handler)
                  (push (cons args handler) commands))))
       ,@body)))

(provide 'elfin-test-helper)
;;; elfin-test-helper.el ends here
