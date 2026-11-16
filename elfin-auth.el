;;; elfin-auth.el --- Jellyfin authentication -*- lexical-binding: t -*-

;;; Commentary:

;; Authenticate to Jellyfin.

;;; Code:

(require 'elfin-vars)
(require 'elfin-api)
(eval-when-compile
  (require 'cl-lib))

(defun elfin-auth--ensure-session-file ()
  "Create `elfin-session-file' if it doesn't exist and set permissions to 600."
  (unless (file-exists-p elfin-session-file)
    (make-empty-file elfin-session-file t))
  (set-file-modes elfin-session-file #o600))

(defun elfin-save-sessions ()
  "Save `elfin--sessions' to `elfin-session-file'."
  (when elfin-session-file
    (elfin-auth--ensure-session-file)
    (with-temp-file elfin-session-file
      (prin1 elfin--sessions (current-buffer)))))

(defun elfin-restore-sessions ()
  "Restore sessions from `elfin-session-file'.
When called interactively, prompt for which session to activate.
Otherwise, activate the first session."
  (interactive)
  (when (and elfin-session-file (file-exists-p elfin-session-file))
    (setq elfin--sessions
          (with-temp-buffer
            (insert-file-contents elfin-session-file)
            (read (current-buffer))))
    (setq elfin--active-session
          (if-let* (((and (called-interactively-p 'any) (cdr elfin--sessions)))
                    (candidates
                     (mapcar (lambda (s)
                               (cons (format "%s @ %s"
                                             (plist-get s :username)
                                             (plist-get s :server))
                                     s))
                             elfin--sessions))
                    (choice (completing-read "Activate session: "
                                             candidates nil t)))
              (cdr (assoc choice candidates))
            (car elfin--sessions)))))

;; These functions are variants of the ones in elfin-api that are explicitly
;; designed to be used before we have a session token.
(defun elfin-auth--get (server endpoint &optional params)
  "Make unauthenticated GET request to SERVER at ENDPOINT.
PARAMS is a plist of query parameters, or nil.
Return a future for the parsed JSON response."
  (condition-case err
      (elfin--json-future
       'get
       (concat (string-remove-suffix "/" server) endpoint
               (elfin--plist-to-query-string params))
       `(("Authorization" . ,(elfin--auth-header))))
    (error (futur-failed err))))

(defun elfin-auth--post (server endpoint body)
  "Make unauthenticated POST request to SERVER at ENDPOINT.
BODY is a JSON-serializable plist.
Return a future for the parsed JSON response."
  (condition-case err
      (elfin--json-future
       'post (concat (string-remove-suffix "/" server) endpoint)
       `(("Content-Type" . "application/json")
         ("Authorization" . ,(elfin--auth-header)))
       (json-serialize body))
    (error (futur-failed err))))

(defun elfin-auth--prompt (function)
  "Run prompting FUNCTION on a timer and return a future for its result.
Do not block Futur's continuation worker while waiting for user input.
Quitting the prompt cancels the future."
  (futur-new
   (lambda (future)
     (cons 'timer
           (run-at-time
            0 nil
            (lambda ()
              (condition-case err
                  (let ((inhibit-quit nil))
                    (futur-deliver-value future (funcall function)))
                (quit (futur-deliver-failure future '(futur-aborted "Authentication cancelled")))
                (error (futur-deliver-failure future err)))))))))

(defun elfin-auth--do-authenticate (server user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
Save the session in `elfin--sessions' on success."
  (elfin--consume-future
   (futur-let* ((response <- (elfin-auth--post server "/Users/AuthenticateByName"
                                               `(:Username ,user :Pw ,pass))))
     (let* ((access-token (gethash "AccessToken" response))
            (user-data (gethash "User" response))
            (user-id (gethash "Id" user-data))
            (session `(:server ,(string-remove-suffix "/" server)
                               :user-id ,user-id
                               :access-token ,access-token
                               :username ,user)))
       (push session elfin--sessions)
       (setq elfin--active-session session)
       (elfin-save-sessions)
       (message "Authenticated as %s on %s" user server)))
   (format "Authentication failed for %s" user)))

(defun elfin-authenticate (server &optional user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
When called interactively, prompt for the server URL and then
fetch the public user list for completion. USER and PASS may be
provided for non-interactive use."
  (interactive
   (list (read-string "Jellyfin server URL: ")))
  (if (and user pass)
      (elfin-auth--do-authenticate server user pass)
    (elfin--consume-future
     (futur-let* ((response <- (elfin-auth--get server "/Users/Public"))
                  (credentials <-
                               (elfin-auth--prompt
                                (lambda ()
                                  (let* ((names (seq-map (lambda (u) (gethash "Name" u)) response))
                                         (user (completing-read "Username: " names nil nil))
                                         (pass (read-passwd "Password: ")))
                                    (list user pass))))))
       (elfin-auth--do-authenticate server (car credentials) (cadr credentials)))
     (format "Could not reach server %s" server))))

(defun elfin-auth--qc-poll (server secret code)
  "Poll Quick Connect status for SECRET on SERVER.
CODE is the user-facing code shown in messages."
  (message "Checking Quick Connect code...")
  (elfin--consume-future
   (futur-let* ((response <- (elfin-auth--get server "/QuickConnect/Connect"
                                              `(:secret ,secret))))
     (if (eq (gethash "Authenticated" response) t)
         (elfin-auth--qc-finish server secret)
       (futur-let* ((_ <- (elfin-auth--prompt
                           (lambda ()
                             (read-string
                              (format "Not yet approved (code: %s). Press enter to recheck: " code))))))
         (elfin-auth--qc-poll server secret code))))
   "Quick Connect polling failed"))

(defun elfin-auth--qc-finish (server secret)
  "Exchange Quick Connect SECRET for a session token on SERVER."
  (elfin--consume-future
   (futur-let* ((response <- (elfin-auth--post server "/Users/AuthenticateWithQuickConnect"
                                               `(:Secret ,secret))))
     (let* ((access-token (gethash "AccessToken" response))
            (user-data (gethash "User" response))
            (user-id (gethash "Id" user-data))
            (username (gethash "Name" user-data))
            (session `(:server ,(string-remove-suffix "/" server)
                               :user-id ,user-id
                               :access-token ,access-token
                               :username ,username)))
       (push session elfin--sessions)
       (setq elfin--active-session session)
       (elfin-save-sessions)
       (message "Authenticated as %s on %s via Quick Connect" username server)))
   "Quick Connect authentication failed"))

(defun elfin-quick-connect (server)
  "Authenticate with Jellyfin SERVER using Quick Connect.
Initiates a Quick Connect request, displays the code, and waits
for the user to approve it in the Jellyfin dashboard."
  (interactive
   (list (read-string "Jellyfin server URL: ")))
  (elfin--consume-future
   (futur-let* ((response <- (elfin-auth--post server "/QuickConnect/Initiate" nil))
                (secret (gethash "Secret" response))
                (code (gethash "Code" response))
                (_ <- (elfin-auth--prompt
                       (lambda ()
                         (kill-new code)
                         (read-string
                          (format "Quick Connect code: %s (copied). Press enter when approved: " code))))))
     (elfin-auth--qc-poll server secret code))
   (format "Quick Connect not available on %s" server)))

(defun elfin-delete-session ()
  "Delete a session from `elfin--sessions'.
Prompt for which session to delete. If the deleted session was
active, clear `elfin--active-session'."
  (interactive)
  (unless elfin--sessions
    (user-error "No sessions to delete"))
  (let* ((candidates
          (mapcar (lambda (s)
                    (cons (format "%s @ %s"
                                  (plist-get s :username)
                                  (plist-get s :server))
                          s))
                  elfin--sessions))
         (choice (completing-read "Delete session: " candidates nil t))
         (session (cdr (assoc choice candidates))))
    (setq elfin--sessions (delq session elfin--sessions))
    (when (equal elfin--active-session session)
      (setq elfin--active-session nil))
    (elfin-save-sessions)
    (message "Deleted session: %s" choice)))

(defun elfin-delete-all-sessions (no-confirm)
  "Delete all sessions from `elfin--sessions'.
Prompt the user for confirmation unless NO-CONFIRM is non-nil."
  (interactive "P")
  (unless elfin--sessions
    (user-error "No sessions to delete"))
  (when (or no-confirm
            (yes-or-no-p
             (format "Delete all %d session(s)?" (length elfin--sessions))))
    (setq elfin--sessions nil
          elfin--active-session nil)
    (elfin-save-sessions)
    (message "All sessions deleted")))

(provide 'elfin-auth)

;;; elfin-auth.el ends here
