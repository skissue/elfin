;;; elfin-auth.el --- Jellyfin authentication -*- lexical-binding: t -*-

;;; Commentary:

;; Authenticate to Jellyfin.

;;; Code:

(require 'elfin-api)

(defmacro elfin--preauth-get (server endpoint params &rest then)
  "Make unauthenticated GET request to SERVER at ENDPOINT.
PARAMS is a plist of query parameters, or nil. THEN is evaluated
with the response data bound to `response'."
  (declare (indent 2))
  `(let* ((query-string (elfin--plist-to-query-string ,params))
          (url (concat (string-remove-suffix "/" ,server) ,endpoint query-string)))
     (plz 'get url
       :headers `(("Authorization" . ,(elfin--auth-header)))
       :as #'json-parse-buffer
       :then (lambda (response)
               (elfin--api-log (format "GET %s -> %S" url response))
               ,@then)
       :else (lambda (err)
               (elfin--api-log (format "GET %s -> ERROR %S" url err))
               (message "Request failed: %S" err)))))

(defmacro elfin--preauth-post (server endpoint body &rest then)
  "Make unauthenticated POST request to SERVER at ENDPOINT.
BODY is a JSON-serializable plist. THEN is evaluated with the
response data bound to `response'."
  (declare (indent 2))
  `(let ((url (concat (string-remove-suffix "/" ,server) ,endpoint)))
     (plz 'post url
       :headers `(("Content-Type" . "application/json")
                  ("Authorization" . ,(elfin--auth-header)))
       :body (json-serialize ,body)
       :as #'json-parse-buffer
       :then (lambda (response)
               (elfin--api-log (format "POST %s -> %S" url response))
               ,@then)
       :else (lambda (err)
               (elfin--api-log (format "POST %s -> ERROR %S" url err))
               (message "Request failed: %S" err)))))

(defun elfin-authenticate (server user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
Save the session in `elfin--sessions'."
  (interactive
   (list (read-string "Jellyfin server URL: ")
         (read-string "Username: ")
         (read-passwd "Password: ")))
  (elfin--preauth-post server "/Users/AuthenticateByName"
    `(:Username ,user :Pw ,pass)
    (let* ((access-token (gethash "AccessToken" response))
           (user-data (gethash "User" response))
           (user-id (gethash "Id" user-data))
           (session `(:server ,(string-remove-suffix "/" server)
                              :user-id ,user-id
                              :access-token ,access-token
                              :username ,user)))
      (push session elfin--sessions)
      (setq elfin--active-session session)
      (message "Authenticated as %s on %s" user server))))

(provide 'elfin-auth)

;;; elfin-auth.el ends here
