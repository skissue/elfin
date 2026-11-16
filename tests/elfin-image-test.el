;;; elfin-image-test.el --- Image cache regression tests -*- lexical-binding: t -*-

(require 'ert)
(require 'elfin)
(require 'elfin-test-helper)

(defmacro elfin-test-with-images (&rest body)
  "Run BODY with disposable cache, captured `requests', and deferred timers.
Call the local function `flush-timers' to deliver scheduled callbacks."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "elfin-images-" t))
          (elfin-image-cache-directory directory)
          (elfin--active-session '(:server "https://jellyfin.invalid/" :access-token "image-token"))
          (elfin--image-in-flight (make-hash-table :test #'equal))
          (elfin--image-download-queue (make-plz-queue :limit 4))
          (elfin-image-download-concurrency 2)
          requests timers)
     (unwind-protect
         (cl-letf (((symbol-function 'plz-queue)
                    (lambda (_queue method url &rest options)
                      (push (list method url options) requests)))
                   ((symbol-function 'plz-run) #'ignore)
                   ((symbol-function 'run-at-time)
                    (lambda (_time _repeat function &rest args)
                      (push (cons function args) timers))))
           (cl-labels ((flush-timers ()
                         (while timers (apply #'funcall (pop timers)))))
             ,@body))
       (delete-directory directory t))))

(ert-deftest elfin-image-owner-precedence-and-cache-separation ()
  (elfin-test-with-images
    ;; Keep mutations local even when the compiler constant-folds JSON parsing.
    (let* ((item (copy-hash-table
                  (json-parse-string
                   "{\"Id\":\"track\",\"ImageTags\":{\"Primary\":\"own\"},\"AlbumId\":\"album\",\"AlbumPrimaryImageTag\":\"album-tag\",\"ParentPrimaryImageItemId\":\"parent\",\"ParentPrimaryImageTag\":\"parent-tag\"}")))
           (own (elfin--ensure-image-file item 80 45))
           paths)
      (should (string-match-p "/Items/track/Images/Primary?" (nth 1 (car requests))))
      (should (equal (cdr (assoc "tag" (elfin-test-query (car requests)))) '("own")))
      (remhash "ImageTags" item)
      (push (elfin--ensure-image-file item 80 45) paths)
      (should (string-match-p "/Items/album/Images/Primary?" (nth 1 (car requests))))
      (remhash "AlbumPrimaryImageTag" item)
      (push (elfin--ensure-image-file item 80 45) paths)
      (should (string-match-p "/Items/parent/Images/Primary?" (nth 1 (car requests))))
      (push (elfin--ensure-image-file item 45 80) paths)
      (push (elfin--ensure-image-file item 81 45) paths)
      (push (elfin--ensure-image-file item 80 46) paths)
      (puthash "ParentPrimaryImageTag" "new-tag" item)
      (push (elfin--ensure-image-file item 80 45) paths)
      (puthash "ParentPrimaryImageItemId" "other-parent" item)
      (push (elfin--ensure-image-file item 80 45) paths)
      (let ((elfin--active-session '(:server "https://other.invalid")))
        (push (elfin--ensure-image-file item 80 45) paths))
      (should (= (length (delete-dups (cons own paths))) 9)))))

(ert-deftest elfin-image-download-coalesces-and-publishes-before-callback ()
  (elfin-test-with-images
    (let* ((first (json-parse-string "{\"Id\":\"track-a\",\"AlbumId\":\"album\",\"AlbumPrimaryImageTag\":\"tag & one\"}"))
           (second (json-parse-string "{\"Id\":\"track-b\",\"AlbumId\":\"album\",\"AlbumPrimaryImageTag\":\"tag & one\"}"))
           ready
           (callback (lambda (path)
                       (should (file-regular-p path))
                       (push path ready)))
           (path (elfin--ensure-image-file first 80 45 callback)))
      (should (equal (elfin--ensure-image-file second 80 45 callback) path))
      (should-not (file-exists-p path))
      (should-not ready)
      (should (= (length requests) 1))
      (should (= (plz-queue-limit elfin--image-download-queue) 2))
      (let* ((request (pop requests))
             (options (nth 2 request))
             (temporary (cadr (plist-get options :as))))
        (should (eq (car request) 'get))
        (should (= (plist-get options :timeout) 30))
        (should (string-match-p "Token=image-token" (cdr (assoc "Authorization" (plist-get options :headers)))))
        (should (equal (sort (elfin-test-query request) (lambda (a b) (string< (car a) (car b))))
                       '(("format" "Jpg") ("maxHeight" "45") ("maxWidth" "80") ("tag" "tag & one"))))
        (with-temp-file temporary (insert "complete image bytes"))
        (funcall (plist-get options :then) temporary)
        (should-not ready)
        (should-not (file-exists-p temporary))
        (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                       "complete image bytes"))
        (flush-timers)
        (should (equal ready (list path path))))
      ;; A warm cache still delivers asynchronously and never queues HTTP.
      (should (equal (elfin--ensure-image-file first 80 45 callback) path))
      (should (= (length ready) 2))
      (should-not requests)
      (flush-timers)
      (should (= (length ready) 3)))))

(ert-deftest elfin-image-download-errors-clean-partials-and-allow-retry ()
  (elfin-test-with-images
    (let* ((item (json-parse-string "{\"Id\":\"track\",\"ImageTags\":{\"Primary\":\"tag\"}}"))
           failures ready
           (on-error (lambda (path err) (push (list path err) failures)))
           (path (elfin--ensure-image-file item 80 45 nil on-error)))
      (elfin--ensure-image-file item 80 45 nil on-error)
      (let* ((options (nth 2 (pop requests)))
             (temporary (cadr (plist-get options :as))))
        (with-temp-file temporary (insert "partial"))
        (funcall (plist-get options :else) 'timeout)
        (should-not failures)
        (should-not (file-exists-p temporary))
        (should-not (file-exists-p path))
        (flush-timers)
        (should (equal failures (list (list path 'timeout) (list path 'timeout)))))
      (should (equal (elfin--ensure-image-file item 80 45 (lambda (p) (setq ready p))) path))
      (should (= (length requests) 1))
      (let* ((options (nth 2 (pop requests)))
             (temporary (cadr (plist-get options :as))))
        (with-temp-file temporary (insert "retry succeeded"))
        (funcall (plist-get options :then) temporary))
      (flush-timers)
      (should (equal ready path)))))

(ert-deftest elfin-image-publish-failure-reports-error-and-allows-retry ()
  (elfin-test-with-images
    (let* ((item (json-parse-string "{\"Id\":\"track\",\"ImageTags\":{\"Primary\":\"tag\"}}"))
           failure
           (path (elfin--ensure-image-file item 80 45 nil
                                          (lambda (_ err) (setq failure err))))
           (options (nth 2 (pop requests)))
           (temporary (cadr (plist-get options :as))))
      (with-temp-file temporary (insert "downloaded"))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (signal 'file-error '("cannot publish")))))
        (funcall (plist-get options :then) temporary))
      (should-not (file-exists-p temporary))
      (should-not (file-exists-p path))
      (flush-timers)
      (should (eq (car failure) 'file-error))
      (elfin--ensure-image-file item 80 45)
      (should (= (length requests) 1)))))

(ert-deftest elfin-image-missing-metadata-and-server-are-deferred-errors ()
  (elfin-test-with-images
    (let (failures)
      (should-not (elfin--ensure-image-file (json-parse-string "{\"Id\":\"bare\"}") 80 45 nil
                                           (lambda (path err) (push (list path err) failures))))
      (let ((elfin--active-session nil))
        (should-not (elfin--ensure-image-file
                     (json-parse-string "{\"Id\":\"track\",\"ImageTags\":{\"Primary\":\"tag\"}}")
                     80 45 nil (lambda (path err) (push (list path err) failures)))))
      (should-not failures)
      (should-not requests)
      (flush-timers)
      (should (member '(nil no-image) failures))
      (should (member '(nil no-active-server) failures)))))

(ert-deftest elfin-image-setup-errors-allow-another-request ()
  "Report setup errors asynchronously without abandoning later requests."
  (let* ((directory (make-temp-file "elfin-image-test-" t))
         (elfin-image-cache-directory (expand-file-name "cache" directory))
         (elfin--active-session '(:server "https://example.invalid"))
         (elfin--image-in-flight (make-hash-table :test #'equal))
         (elfin--image-download-queue (make-plz-queue :limit 1))
         (item (json-parse-string
                "{\"Id\":\"track\",\"ImageTags\":{\"Primary\":\"tag\"}}")))
    (unwind-protect
        (progn
          ;; A file where the cache directory should be makes setup fail.
          (with-temp-file elfin-image-cache-directory (insert "not a directory"))
          (dotimes (_ 2)
            (let* (failures
                   (path (elfin--ensure-image-file
                          item 64 64 nil
                          (lambda (path err) (push (cons path err) failures))))
                   (deadline (+ (float-time) 1)))
              (should (stringp path))
              (should-not failures)
              (while (and (null failures) (< (float-time) deadline))
                (accept-process-output nil 0.01))
              (should (= (length failures) 1))
              (should (equal (caar failures) path))
              (should (memq 'file-error
                            (get (cadar failures) 'error-conditions))))))
      (delete-directory directory t))))

;;; elfin-image-test.el ends here
