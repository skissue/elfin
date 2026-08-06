;;; elfin-image.el --- Jellyfin image caching -*- lexical-binding: t -*-

;;; Commentary:

;; Resolve Jellyfin image metadata and cache images on disk.

;;; Code:

(require 'elfin-api)
(require 'elfin-vars)
(require 'plz)

(defgroup elfin-image nil
  "Jellyfin image retrieval and caching."
  :group 'elfin
  :prefix "elfin-image-")

(defcustom elfin-image-cache-directory
  (expand-file-name
   "elfin/images/"
   (or (getenv "XDG_CACHE_HOME")
       (expand-file-name "~/.cache/")))
  "Directory containing cached Jellyfin images."
  :type 'directory)

(defcustom elfin-image-download-concurrency 4
  "Maximum number of simultaneous image downloads."
  :type 'integer)

(defvar elfin--image-in-flight (make-hash-table :test #'equal)
  "Callbacks waiting for each image currently being downloaded.")

(defvar elfin--image-download-queue
  (make-plz-queue :limit elfin-image-download-concurrency)
  "Queue controlling concurrent image downloads.")

(defun elfin--image-call-later (function &rest arguments)
  "Call FUNCTION with ARGUMENTS asynchronously when FUNCTION is non-nil."
  (when function
    (apply #'run-at-time 0 nil function arguments)))

(defun elfin--image-info (item)
  "Return (OWNER-ID . TAG) for ITEM's primary image, or nil."
  (let* ((tags (gethash "ImageTags" item))
         (primary-tag (and tags (gethash "Primary" tags))))
    (cond
     (primary-tag
      (cons (gethash "Id" item) primary-tag))
     ((and (gethash "AlbumId" item)
           (gethash "AlbumPrimaryImageTag" item))
      (cons (gethash "AlbumId" item)
            (gethash "AlbumPrimaryImageTag" item)))
     ((and (gethash "ParentPrimaryImageItemId" item)
           (gethash "ParentPrimaryImageTag" item))
      (cons (gethash "ParentPrimaryImageItemId" item)
            (gethash "ParentPrimaryImageTag" item))))))

(defun elfin--image-cache-path (server owner-id tag width height)
  "Return cache path for an image identified by SERVER, OWNER-ID and TAG.
WIDTH and HEIGHT identify the requested rendition."
  (let ((key (secure-hash
              'sha256
              (mapconcat #'identity
                         (mapcar (lambda (value) (format "%s" value))
                                 (list server owner-id tag width height "Jpg"))
                         "\0"))))
    (expand-file-name (concat key ".jpg")
                      elfin-image-cache-directory)))

(defun elfin--image-request-url (server owner-id tag width height)
  "Return image URL on SERVER for OWNER-ID and TAG at WIDTH by HEIGHT."
  (concat (directory-file-name server)
          "/Items/" owner-id "/Images/Primary"
          (elfin--plist-to-query-string
           `(:tag ,tag
                  :maxWidth ,width
                  :maxHeight ,height
                  :format "Jpg"))))

(defun elfin--image-notify-ready (path)
  "Notify all callbacks waiting for the completed image at PATH."
  (let ((callbacks (gethash path elfin--image-in-flight)))
    (remhash path elfin--image-in-flight)
    (dolist (callback callbacks)
      (elfin--image-call-later (car callback) path))))

(defun elfin--image-notify-error (path error)
  "Notify all callbacks waiting for PATH that ERROR occurred."
  (let ((callbacks (gethash path elfin--image-in-flight)))
    (remhash path elfin--image-in-flight)
    (dolist (callback callbacks)
      (elfin--image-call-later (cdr callback) path error))))

(defun elfin--image-enqueue-download (path url token)
  "Enqueue URL for download to eventual PATH using TOKEN.
Call `plz-run' separately to start processing the queue."
  (make-directory elfin-image-cache-directory t)
  (let ((temporary
         (make-temp-name
          (expand-file-name ".download-" elfin-image-cache-directory))))
    (setf (plz-queue-limit elfin--image-download-queue)
          (max 1 elfin-image-download-concurrency))
    (plz-queue elfin--image-download-queue 'get url
      :headers `(("Authorization" . ,(elfin--auth-header token)))
      :as `(file ,temporary)
      :timeout 30
      :then (lambda (_downloaded-file)
              (condition-case error
                  (progn
                    (rename-file temporary path t)
                    (elfin--image-notify-ready path))
                (error
                 (when (file-exists-p temporary)
                   (delete-file temporary))
                 (elfin--image-notify-error path error))))
      :else (lambda (error)
              (when (file-exists-p temporary)
                (delete-file temporary))
              (elfin--image-notify-error path error)))))

(defun elfin--ensure-image-file
    (item width height &optional on-ready on-error)
  "Ensure ITEM's primary image is cached and return its eventual path.

WIDTH and HEIGHT specify the maximum dimensions of the cached image.
The returned path may not exist yet.  ON-READY is called
asynchronously with the path after the complete file exists.
ON-ERROR is called asynchronously with the path and error if the
setup or download fails.  Return nil when ITEM has no tagged image
or there is no active server."
  (let* ((info (and item (elfin--image-info item)))
         (server (plist-get elfin--active-session :server))
         (token (plist-get elfin--active-session :access-token)))
    (if (not (and info server))
        (progn
          (elfin--image-call-later on-error nil
                                   (if info 'no-active-server 'no-image))
          nil)
      (let* ((owner-id (car info))
             (tag (cdr info))
             (path (elfin--image-cache-path server owner-id tag width height)))
        (cond
         ((file-regular-p path)
          (elfin--image-call-later on-ready path))
         ((gethash path elfin--image-in-flight)
          (puthash path
                   (cons (cons on-ready on-error)
                         (gethash path elfin--image-in-flight))
                   elfin--image-in-flight))
         (t
          (puthash path (list (cons on-ready on-error))
                   elfin--image-in-flight)
          (when (condition-case err
                    (progn
                      (elfin--image-enqueue-download
                       path
                       (elfin--image-request-url server owner-id tag width height)
                       token)
                      t)
                  (error
                   (elfin--image-notify-error path err)
                   nil))
            (plz-run elfin--image-download-queue))))
        path))))

(provide 'elfin-image)

;;; elfin-image.el ends here
