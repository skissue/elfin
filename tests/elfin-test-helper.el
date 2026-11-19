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
                  (push (list method url options) requests))))
       ,@body)))

(defun elfin-test-respond (request json &optional failure)
  "Deliver JSON to captured REQUEST, or FAILURE to its error callback."
  (funcall (plist-get (nth 2 request) (if failure :else :then))
           (if failure failure (json-parse-string json))))

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
         (elfin--current-entry-id nil)
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
