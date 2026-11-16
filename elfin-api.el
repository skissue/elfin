;;; elfin-api.el --- Jellyfin API interaction -*- lexical-binding: t -*-

;;; Commentary:

;; API interaction and authentication for Jellyfin.

;;; Code:

(require 'elfin-vars)
(require 'plz)
(require 'futur)

(defconst elfin--client-name "elfin"
  "Client name sent to Jellyfin server.")

(defconst elfin--client-version "0.0.1"
  "Client version sent to Jellyfin server.")

(defconst elfin--device-name "Emacs"
  "Device name sent to Jellyfin server.")

(defvar elfin--sessions nil
  "List of known Jellyfin sessions.
Each session is a plist with :server, :user-id, :access-token,
:username.")

(defvar elfin--active-session nil
  "Plist containing the currently active session info.")

(defun elfin--api-log (string)
  "When `elfin-debug' is non-nil, log STRING to *Elfin API Log*."
  (when elfin-debug
    (with-current-buffer (get-buffer-create "*Elfin API Log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "[%T] ")
                string "\n")))))

(defun elfin--auth-header (&optional token)
  "Generate the X-Emby-Authorization header value.
If TOKEN is provided, include it in the header."
  ;; NOTE plz.el currently cannot handle quotes in headers, so do not quote
  ;; these values.
  (format "MediaBrowser Client=%s, Device=%s, DeviceId=%s, Version=%s%s"
          elfin--client-name
          elfin--device-name
          (system-name)
          elfin--client-version
          (if token (format ", Token=%s" token) "")))

(defun elfin--plist-to-query-string (plist)
  "Convert PLIST to a URL query string."
  (let (pairs)
    (map-do
     (lambda (key value)
       (push (format "%s=%s"
                     (url-hexify-string (substring (symbol-name key) 1))
                     (url-hexify-string
                      (pcase value
                        ('nil "false")
                        ('t "true")
                        (_ (format "%s" value)))))
             pairs))
     plist)
    (when pairs
      (concat "?" (string-join pairs "&")))))

(defun elfin--json-future (method url headers &optional body)
  "Return a future for parsed JSON from METHOD at URL using plz.
HEADERS and BODY are passed to plz unchanged.  Transport, request setup,
and JSON parsing errors reject the future.  Log results without reporting
errors to the user; consumers own terminal error handling."
  (futur-bind
   (condition-case err
       (futur-bind
        (futur-new
         (lambda (future)
           (plz method url :headers headers :body body :as 'string
             :then (lambda (text)
                     (futur-deliver-value future text))
             :else (lambda (err)
                     (futur-deliver-failure future (list 'plz-error err))))))
        #'json-parse-string)
     (error (futur-failed err)))
   (lambda (response)
     (elfin--api-log (format "%s %s -> %S" (upcase (symbol-name method)) url response))
     response)
   (lambda (err)
     (elfin--api-log (format "%s %s -> ERROR %S" (upcase (symbol-name method)) url
                            (if (eq (car err) 'plz-error) (cadr err) err)))
     (futur-failed err))))

(defun elfin--get (endpoint &optional params)
  "Return a future for authenticated JSON from ENDPOINT.
ENDPOINT is relative to the server URL.  PARAMS is a plist of query
parameters.  Without an active session, return a failed future."
  (condition-case err
      (if-let* ((session elfin--active-session))
          (elfin--json-future
           'get
           (concat (plist-get session :server) endpoint
                   (elfin--plist-to-query-string params))
           `(("Authorization" . ,(elfin--auth-header
                                  (plist-get session :access-token)))))
        (futur-failed '(user-error "No active Jellyfin session")))
    (error (futur-failed err))))

(defun elfin--consume-future (future &optional error-context)
  "Consume FUTURE, reporting failures once and ignoring cancellation.
ERROR-CONTEXT, when non-nil, replaces the default failure message prefix.
Use at command boundaries where no caller will handle a failed future.
Return a future resolving to nil after the entire operation finishes."
  (futur-bind future #'ignore
              (lambda (err)
                (unless (eq (car err) 'futur-aborted)
                  (message "%s: %s"
                           (or error-context
                               (if (eq (car err) 'plz-error) "Request failed" "Elfin"))
                           (if (eq (car err) 'plz-error)
                               (format "%S" (cadr err))
                             (error-message-string err))))
                nil)))

(defun elfin--audio-url (id)
  "Return the audio stream URL for item ID."
  (format "%s/Audio/%s/universal?ApiKey=%s&Container=opus,webm|opus,ts|mp3,mp3,flac,webma,webm|webma,wav,ogg&TranscodingContainer=ts&TranscodingProtocol=hls&AudioCodec=opus"
          (plist-get elfin--active-session :server)
          id
          (plist-get elfin--active-session :access-token)))

(defun elfin--image-url (id width height)
  "Return the image URL for item ID with dimensions WIDTH x HEIGHT."
  (format "%s/Items/%s/Images/Primary?maxWidth=%d&maxHeight=%d"
          (plist-get elfin--active-session :server)
          id
          width height))

(defun elfin--fetch-thumbnails (ids width height on-success on-error)
  "Fetch thumbnails for IDS with dimensions WIDTH x HEIGHT.
ON-SUCCESS is called with (id data) for each successful fetch.
ON-ERROR is called with (id err) for each failure."
  (let ((queue (make-plz-queue :limit 4)))
    (plz-run
     (dolist (id ids queue)
       (plz-queue queue 'get (elfin--image-url id width height)
         :as 'binary
         :then (lambda (data) (funcall on-success id data))
         :else (lambda (err) (funcall on-error id err)))))))

(defun elfin--get-child-items (parent-id)
  "Return a future containing PARENT-ID's audio item IDs in server order.
An empty collection resolves to nil."
  (futur-let* ((response <- (elfin--get "/Items" `(:parentId ,parent-id
                                                :includeItemTypes "Audio"
                                                :Recursive t))))
    (seq-map (lambda (item) (gethash "Id" item))
             (gethash "Items" response))))

(provide 'elfin-api)

;;; elfin-api.el ends here
