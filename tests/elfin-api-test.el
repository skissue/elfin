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
    (let (received)
      (elfin--get "/Items" '(:limit 7) (setq received response))
      (let* ((request (pop requests))
             (headers (plist-get (nth 2 request) :headers)))
        (should (eq (car request) 'get))
        (should (equal (nth 1 request) "https://jellyfin.invalid/Items?limit=7"))
        (should (string-match-p "Token=test-token" (cdr (assoc "Authorization" headers))))
        (should-not received)
        (elfin-test-respond request "{\"Items\":[{\"Id\":\"a\"}]}")
        (should (equal (gethash "Id" (aref (gethash "Items" received) 0)) "a"))))))

(ert-deftest elfin-api-get-failure-does-not-deliver-success ()
  (elfin-test-with-http
    (let (received messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (elfin--get "/Items" nil (setq received response))
        (elfin-test-respond (pop requests) nil 'offline)
        (should-not received)
        (should (string-match-p "Request failed.*offline" (car messages)))))))

(ert-deftest elfin-api-get-requires-session ()
  (elfin-test-with-http
    (let ((elfin--active-session nil))
      (should-error (elfin--get "/Items" nil response))
      (should-not requests))))

(ert-deftest elfin-api-media-urls ()
  (elfin-test-with-http
    (let ((audio (elfin--audio-url "track-23")))
      (should (string-prefix-p "https://jellyfin.invalid/Audio/track-23/universal?" audio))
      (should (string-match-p "ApiKey=test-token" audio))
      (should (string-match-p "TranscodingProtocol=hls" audio)))
    (should (equal (elfin--image-url "album-9" 80 45)
                   "https://jellyfin.invalid/Items/album-9/Images/Primary?maxWidth=80&maxHeight=45"))))

(ert-deftest elfin-api-child-items-preserve-order-and-skip-empty ()
  (elfin-test-with-http
    (let ((received 'unchanged))
      (elfin--get-child-items "album" (setq received ids))
      (let ((request (pop requests)))
        (should (equal (cdr (assoc "parentId" (elfin-test-query request))) '("album")))
        (should (equal (cdr (assoc "includeItemTypes" (elfin-test-query request))) '("Audio")))
        (elfin-test-respond request "{\"Items\":[{\"Id\":\"z\"},{\"Id\":\"a\"}]}")
        (should (equal received '("z" "a"))))
      (elfin--get-child-items "empty" (setq received ids))
      (elfin-test-respond (pop requests) "{\"Items\":[]}")
      (should (equal received '("z" "a"))))))

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
