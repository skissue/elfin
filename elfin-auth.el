;;; elfin-auth.el --- Jellyfin authentication -*- lexical-binding: t -*-

;;; Commentary:

;; Authenticate to Jellyfin.

;;; Code:

(require 'elfin-api)
(eval-when-compile
  (require 'cl-lib))

(defmacro elfin--preauth-get (server endpoint params &rest body)
  "Make unauthenticated GET request to SERVER at ENDPOINT.
PARAMS is a plist of query parameters, or nil. BODY is evaluated
with the response data bound to `response'. If BODY contains
`:else FORM', FORM is used as the error handler with `err' bound;
otherwise a default message is shown."
  (declare (indent defun))
  (let* ((else-pos (cl-position :else body))
         (then (if else-pos (cl-subseq body 0 else-pos) body))
         (else-form (when else-pos (nth (1+ else-pos) body))))
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
                 ,(or else-form '(message "Request failed: %S" err)))))))

(defmacro elfin--preauth-post (server endpoint body &rest rest)
  "Make unauthenticated POST request to SERVER at ENDPOINT.
BODY is a JSON-serializable plist. REST is evaluated with the
response data bound to `response'. If REST contains `:else FORM',
FORM is used as the error handler with `err' bound; otherwise a
default message is shown."
  (declare (indent defun))
  (let* ((else-pos (cl-position :else rest))
         (then (if else-pos (cl-subseq rest 0 else-pos) rest))
         (else-form (when else-pos (nth (1+ else-pos) rest))))
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
                 ,(or else-form '(message "Request failed: %S" err)))))))

(defun elfin--do-authenticate (server user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
Save the session in `elfin--sessions' on success."
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
      (message "Authenticated as %s on %s" user server))
    :else (message "Authentication failed for %s: %S" user err)))

(defun elfin-authenticate (server &optional user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
When called interactively, prompt for the server URL and then
fetch the public user list for completion. USER and PASS may be
provided for non-interactive use."
  (interactive
   (list (read-string "Jellyfin server URL: ")))
  (if (and user pass)
      (elfin--do-authenticate server user pass)
    (elfin--preauth-get server "/Users/Public" nil
      (let* ((names (seq-map (lambda (u) (gethash "Name" u)) response))
             (user (completing-read "Username: " names nil nil))
             (pass (read-passwd "Password: ")))
        (elfin--do-authenticate server user pass))
      :else (message "Could not reach server %s: %S" server err))))

(provide 'elfin-auth)

;;; elfin-auth.el ends here
