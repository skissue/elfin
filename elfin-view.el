;;; elfin-view.el --- Item listing views -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated list views for Jellyfin items.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'elfin-api)
(require 'elfin-playback)

;;; Buffer-local state

(defvar-local elfin--view-type nil
  "Symbol identifying the current view type (e.g. `playlists').")

(defvar-local elfin--view-parent-id nil
  "Parent ID used to filter items in the current view.")

(defvar-local elfin--current-page 1
  "Current page number in collection buffer.")

;;; Generic dispatch

(cl-defgeneric elfin-view-fields (type)
  "Return list of field symbols for view TYPE.")

(cl-defgeneric elfin-view-params (type)
  "Return API query parameter plist for view TYPE.")

(cl-defgeneric elfin-view-buffer-name (type)
  "Return buffer name string for view TYPE.")

(cl-defgeneric elfin-view-open (type id)
  "Open item ID in view TYPE.")

(cl-defgeneric elfin-view-queue (type id)
  "Queue item ID in view TYPE.")

(cl-defgeneric elfin-view-play (type id)
  "Play item ID in view TYPE, overriding the current queue.")

;;; View type definitions

;; playlists
(cl-defmethod elfin-view-fields ((_type (eql playlists))) '(name count duration))
(cl-defmethod elfin-view-params ((_type (eql playlists))) '(:includeItemTypes "Playlist" :Recursive t))
(cl-defmethod elfin-view-buffer-name ((_type (eql playlists))) "*Elfin Playlists*")
(cl-defmethod elfin-view-open ((_type (eql playlists)) id) (elfin--show-view 'tracks nil id))
(cl-defmethod elfin-view-queue ((_type (eql playlists)) id) (elfin-queue-collection id))
(cl-defmethod elfin-view-play ((_type (eql playlists)) id) (elfin-play-collection id))

;; albums
(cl-defmethod elfin-view-fields ((_type (eql albums))) '(name artist duration))
(cl-defmethod elfin-view-params ((_type (eql albums))) '(:includeItemTypes "MusicAlbum" :Recursive t))
(cl-defmethod elfin-view-buffer-name ((_type (eql albums))) "*Elfin Albums*")
(cl-defmethod elfin-view-open ((_type (eql albums)) id) (elfin--show-view 'tracks nil id))
(cl-defmethod elfin-view-queue ((_type (eql albums)) id) (elfin-queue-collection id))
(cl-defmethod elfin-view-play ((_type (eql albums)) id) (elfin-play-collection id))

;; artists
(cl-defmethod elfin-view-fields ((_type (eql artists))) '(name count genres))
(cl-defmethod elfin-view-params ((_type (eql artists))) '(:includeItemTypes "MusicArtist" :Recursive t :Fields "ChildCount,Genres"))
(cl-defmethod elfin-view-buffer-name ((_type (eql artists))) "*Elfin Artists*")
(cl-defmethod elfin-view-open ((_type (eql artists)) id) (elfin--show-view 'albums nil id))
(cl-defmethod elfin-view-queue ((_type (eql artists)) id) (elfin-queue-collection id))
(cl-defmethod elfin-view-play ((_type (eql artists)) id) (elfin-play-collection id))

;; tracks
(cl-defmethod elfin-view-fields ((_type (eql tracks))) '(name artists album duration))
(cl-defmethod elfin-view-params ((_type (eql tracks))) '(:includeItemTypes "Audio" :Recursive t))
(cl-defmethod elfin-view-buffer-name ((_type (eql tracks))) "*Elfin Tracks*")
(cl-defmethod elfin-view-open ((_type (eql tracks)) id) (elfin-play-track id))
(cl-defmethod elfin-view-queue ((_type (eql tracks)) id) (elfin-queue-track id))
(cl-defmethod elfin-view-play ((_type (eql tracks)) id) (elfin-play-track id))

;;; Display helpers

(defun elfin--image-column-spec ()
  "Specification for the image column for `tabulated-list-format'."
  (list " "
        (ceiling (/ (float (car elfin-thumbnail-size))
                    (frame-char-width)))
        nil))

(defun elfin--insert-thumbnail (buffer id data)
  "Insert thumbnail DATA for ID into BUFFER at placeholder."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (when (search-forward (format "[[%s]]" id) nil t)
            (delete-region (match-beginning 0) (match-end 0))
            (insert-image (create-image data nil :data))))))))

(defun elfin--format-duration (ticks)
  "Format TICKS (100-nanosecond units) as human-readable duration."
  (if (or (null ticks) (zerop ticks))
      ""
    (let* ((total-seconds (/ ticks 10000000))
           (hours (/ total-seconds 3600))
           (minutes (/ (mod total-seconds 3600) 60))
           (seconds (mod total-seconds 60)))
      (if (> hours 0)
          (format "%d:%02d:%02d" hours minutes seconds)
        (format "%d:%02d" minutes seconds)))))

(defun elfin--field-spec (field)
  "Return (COLUMN-SPEC . EXTRACTOR) for FIELD symbol."
  (pcase field
    ('name '(("Name" 30 t) .
             (lambda (item) (or (gethash "Name" item) "Untitled"))))
    ('artist '(("Artist" 20 t) .
               (lambda (item) (or (gethash "AlbumArtist" item) "Unknown"))))
    ('artists '(("Artist" 20 t) .
                (lambda (item)
                  (let ((artists (gethash "Artists" item)))
                    (if (and artists (> (length artists) 0))
                        (aref artists 0)
                      "Unknown")))))
    ('album '(("Album" 20 t) .
              (lambda (item) (or (gethash "Album" item) "Unknown"))))
    ('duration '(("Duration" 10 t) .
                 (lambda (item) (elfin--format-duration
                            (gethash "RunTimeTicks" item)))))
    ('count '(("Items" 8 t) .
              (lambda (item)
                (number-to-string (or (gethash "ChildCount" item) 0)))))
    ('genres '(("Genres" 20 t) .
               (lambda (item)
                 (let ((genres (gethash "Genres" item)))
                   (if (and genres (> (length genres) 0))
                       (string-join (append genres nil) ", ")
                     "")))))
    (_ (error "Unknown field: %s" field))))

;;; Core view machinery

(defun elfin--display-items (items type page parent-id)
  "Display ITEMS for view TYPE on PAGE with PARENT-ID."
  (let* ((fields (elfin-view-fields type))
         (field-specs (mapcar #'elfin--field-spec fields))
         (columns (vconcat (list (elfin--image-column-spec))
                           (mapcar #'car field-specs)))
         (extractors (mapcar #'cdr field-specs))
         (format-entry
          (lambda (item)
            (let ((id (gethash "Id" item)))
              (list id (vconcat (list (format "[[%s]]" id))
                                (mapcar (lambda (fn) (funcall fn item))
                                        extractors))))))
         (buf (get-buffer-create (elfin-view-buffer-name type))))
    (with-current-buffer buf
      (elfin-items-mode)
      (setq elfin--view-type type
            elfin--view-parent-id parent-id
            elfin--current-page page
            tabulated-list-format columns
            tabulated-list-entries (mapcar format-entry items))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (elfin--fetch-thumbnails
       (mapcar #'car tabulated-list-entries)
       (car elfin-thumbnail-size)
       (cdr elfin-thumbnail-size)
       (lambda (id data) (elfin--insert-thumbnail buf id data))
       (lambda (id err) (message "Error fetching thumbnail for %s: %S" id err))))
    (switch-to-buffer buf)))

(defun elfin--show-view (type &optional page parent-id)
  "Fetch and display view TYPE at PAGE, optionally filtered by PARENT-ID."
  (let* ((page (or page 1))
         (start-index (* (1- page) elfin-max-items-per-page))
         (params (append (elfin-view-params type)
                         (when parent-id `(:parentId ,parent-id))
                         `(:startIndex ,start-index
                                       :limit ,elfin-max-items-per-page))))
    (elfin--get "/Items" params
      (elfin--display-items (gethash "Items" response)
                            type page parent-id))))

;;; Interactive commands

(defun elfin-items-next-page ()
  "Go to next page of items."
  (interactive)
  (elfin--show-view elfin--view-type (1+ elfin--current-page) elfin--view-parent-id))

(defun elfin-items-prev-page ()
  "Go to previous page of items."
  (interactive)
  (if (> elfin--current-page 1)
      (elfin--show-view elfin--view-type (1- elfin--current-page) elfin--view-parent-id)
    (user-error "No previous page")))

(defun elfin-items-open ()
  "Open the item at point."
  (interactive)
  (elfin-view-open elfin--view-type (tabulated-list-get-id)))

(defun elfin-items-queue ()
  "Queue the item at point."
  (interactive)
  (elfin-view-queue elfin--view-type (tabulated-list-get-id)))

(defun elfin-items-play ()
  "Play the item at point, overriding the current queue."
  (interactive)
  (elfin-view-play elfin--view-type (tabulated-list-get-id)))

(define-derived-mode elfin-items-mode tabulated-list-mode "Elfin"
  "Major mode for displaying Jellyfin item lists."
  (setq tabulated-list-padding 2)
  (local-set-key (kbd "N") #'elfin-items-next-page)
  (local-set-key (kbd "P") #'elfin-items-prev-page)
  (local-set-key (kbd "RET") #'elfin-items-open)
  (local-set-key (kbd "a") #'elfin-items-queue)
  (local-set-key (kbd "C-<return>") #'elfin-items-play))

;;; Entry points

(defun elfin-playlists ()
  "List available playlists."
  (interactive)
  (elfin--show-view 'playlists))

(defun elfin-artists ()
  "List available artists."
  (interactive)
  (elfin--show-view 'artists))

(defun elfin-albums ()
  "List available albums."
  (interactive)
  (elfin--show-view 'albums))

(defun elfin-tracks ()
  "List available tracks."
  (interactive)
  (elfin--show-view 'tracks))

(provide 'elfin-view)

;;; elfin-view.el ends here
