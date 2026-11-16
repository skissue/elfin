;;; elfin-api-test.el --- API request contracts -*- lexical-binding: t -*-

(require 'elfin-test-helper)

(ert-deftest elfin-api-query-encoding ()
  (should-not (elfin--plist-to-query-string nil))
  (let ((query (elfin--plist-to-query-string
                '(:searchTerm "A & B/雪" :Recursive t :disabled nil :startIndex 17))))
    (should (string-prefix-p "?" query))
    (should (equal (sort (split-string (substring query 1) "&") #'string<)
                   '("Recursive=true" "disabled=false"
                     "searchTerm=A%20%26%20B%2F%E9%9B%AA" "startIndex=17")))))

(ert-deftest elfin-api-get-authenticates-and-delivers-response ()
  (elfin-test-with-http
    (let ((future (elfin--get "/Items" '(:limit 7))))
      (let* ((request (pop requests))
             (headers (plist-get (nth 2 request) :headers)))
        (should (eq (car request) 'get))
        (should (equal (nth 1 request) "https://jellyfin.invalid/Items?limit=7"))
        (should (string-match-p "Token=test-token" (cdr (assoc "Authorization" headers))))
        (elfin-test-respond request "{\"Items\":[{\"Id\":\"a\"}]}")
        (should (equal (gethash "Id" (aref (gethash "Items" (elfin-test-await future)) 0))
                       "a"))))))

(ert-deftest elfin-api-get-failure-does-not-deliver-success ()
  (elfin-test-with-http
    (let ((failure (make-plz-error :message "offline")) messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (let ((future (elfin--get "/Items")))
          (elfin-test-respond (pop requests) nil failure)
          (should (equal (should-error (elfin-test-await future) :type 'plz-error)
                         (list 'plz-error failure)))
          (should-not messages)
          (should-not (elfin-test-await (elfin--consume-future future)))
          (should (= (length messages) 1))
          (should (string-match-p "Request failed.*offline" (car messages))))))))

(ert-deftest elfin-api-get-requires-session ()
  (elfin-test-with-http
    (let ((elfin--active-session nil))
      (let ((future (elfin--get "/Items")))
        (should-error (elfin-test-await future) :type 'user-error))
      (should-not requests))))

(ert-deftest elfin-api-get-rejects-invalid-json-and-setup-errors ()
  (elfin-test-with-http
    (let ((future (elfin--get "/Items")))
      (elfin-test-respond (pop requests) "not JSON")
      (should-error (elfin-test-await future) :type 'json-parse-error))
    (cl-letf (((symbol-function 'plz) (lambda (&rest _) (error "setup failed"))))
      (let ((future (elfin--get "/Items")))
        (should (equal (should-error (elfin-test-await future))
                       '(error "setup failed")))))))

(ert-deftest elfin-api-get-logs-success-and-failure-only-when-enabled ()
  (elfin-test-with-http
    (let ((buffer (get-buffer-create "*Elfin API Log*")))
      (unwind-protect
          (dolist (enabled '(nil t))
            (with-current-buffer buffer (erase-buffer))
            (let ((elfin-debug enabled)
                  (success (elfin--get "/Success"))
                  (failure (elfin--get "/Failure")))
              (elfin-test-respond (pop requests) nil (make-plz-error :message "offline"))
              (elfin-test-respond (pop requests) "{\"Name\":\"Example\"}")
              (elfin-test-await success)
              (should-error (elfin-test-await failure) :type 'plz-error)
              (with-current-buffer buffer
                (if enabled
                    (progn
                      (should (string-match-p "GET https://jellyfin.invalid/Success -> .*Example"
                                              (buffer-string)))
                      (should (string-match-p "GET https://jellyfin.invalid/Failure -> ERROR .*offline"
                                              (buffer-string))))
                  (should (equal (buffer-string) ""))))))
        (kill-buffer buffer)))))

(ert-deftest elfin-api-terminal-handler-catches-consumer-errors-and-ignores-abort ()
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should-not
       (elfin-test-await
        (elfin--consume-future
         (futur-let* ((value <- (futur-done 7)))
           (error "Consumer failed: %d" value)))))
      (should (equal messages '("Elfin: Consumer failed: 7")))
      (should-not
       (elfin-test-await
        (elfin--consume-future (futur-failed '(futur-aborted "cancelled")))))
      (should (= (length messages) 1)))))

(ert-deftest elfin-api-media-urls ()
  (elfin-test-with-http
    (let ((audio (elfin--audio-url "track-23")))
      (should (string-prefix-p "https://jellyfin.invalid/Audio/track-23/universal?" audio))
      (should (string-match-p "ApiKey=test-token" audio))
      (should (string-match-p "TranscodingProtocol=hls" audio)))
    (should (equal (elfin--image-url "album-9" 80 45)
                   "https://jellyfin.invalid/Items/album-9/Images/Primary?maxWidth=80&maxHeight=45"))))

(ert-deftest elfin-api-child-items-preserve-order-and-return-empty ()
  (elfin-test-with-http
    (let ((future (elfin--get-child-items "album")))
      (let ((request (pop requests)))
        (should (equal (cdr (assoc "parentId" (elfin-test-query request))) '("album")))
        (should (equal (cdr (assoc "includeItemTypes" (elfin-test-query request))) '("Audio")))
        (should (equal (cdr (assoc "Recursive" (elfin-test-query request))) '("true")))
        (elfin-test-respond request "{\"Items\":[{\"Id\":\"z\"},{\"Id\":\"a\"}]}")
        (should (equal (elfin-test-await future) '("z" "a"))))
      (setq future (elfin--get-child-items "empty"))
      (elfin-test-respond (pop requests) "{\"Items\":[]}")
      (should-not (elfin-test-await future)))))

(ert-deftest elfin-api-thumbnail-results-keep-their-item-ids ()
  (let (requests results failures queue)
    (cl-letf (((symbol-function 'plz-queue)
               (lambda (q method url &rest options)
                 (setq queue q)
                 (push (list method url options) requests)))
              ((symbol-function 'plz-run) #'ignore))
      (elfin--fetch-thumbnails '("first" "second") 21 34
                               (lambda (id data) (push (list id data) results))
                               (lambda (id err) (push (list id err) failures)))
      (should (= (plz-queue-limit queue) 4))
      (funcall (plist-get (nth 2 (car requests)) :else) 'not-found)
      (funcall (plist-get (nth 2 (cadr requests)) :then) "bytes")
      (should (equal results '(("first" "bytes"))))
      (should (equal failures '(("second" not-found)))))))

;;; elfin-api-test.el ends here
