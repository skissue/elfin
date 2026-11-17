;;; elfin-view-test.el --- Browsing and buffer contracts -*- lexical-binding: t -*-

(require 'elfin-test-helper)

(ert-deftest elfin-view-truncates-by-default-but-allows-wrapping-hook ()
  (let ((was-enabled global-visual-line-mode))
    (unwind-protect
        (progn
          (global-visual-line-mode 1)
          (dolist (wrap '(nil t))
            (let ((elfin-items-mode-hook
                   (when wrap (list (lambda () (visual-line-mode 1))))))
              (with-temp-buffer
                (elfin-items-mode)
                (should (eq visual-line-mode wrap))
                (should (eq truncate-lines (not wrap)))))))
      (global-visual-line-mode (if was-enabled 1 -1)))))

(ert-deftest elfin-view-duration-boundaries ()
  (dolist (case '((nil "") (0 "") (590000000 "0:59") (600000000 "1:00")
                  (35990000000 "59:59") (36000000000 "1:00:00")
                  (73259999999 "2:02:05")))
    (should (equal (elfin--format-duration (car case)) (cadr case)))))

(ert-deftest elfin-view-entry-points-request-correct-types ()
  (elfin-test-with-http
    (dolist (case '((elfin-albums "MusicAlbum") (elfin-artists "MusicArtist")
                    (elfin-tracks "Audio") (elfin-playlists "Playlist")))
      (funcall (car case))
      (let ((query (elfin-test-query (pop requests))))
        (should (equal (cadr (assoc "includeItemTypes" query)) (cadr case)))
        (should (equal (cadr (assoc "Recursive" query)) "true"))
        (should (equal (cadr (assoc "startIndex" query)) "0"))))))

(ert-deftest elfin-view-pagination-and-search-preserve-parent ()
  (elfin-test-with-http
    (with-temp-buffer
      (elfin-items-mode)
      (setq elfin--view-type 'tracks elfin--view-parent-id "album-4"
            elfin--current-page 3 elfin--view-search-term "old")
      (let ((elfin-max-items-per-page 7))
        (dolist (case '((elfin-items-next-page 21 "old")
                        (elfin-items-prev-page 7 "old")
                        (elfin-items-clear-search 0 nil)))
          (funcall (car case))
          (let ((query (elfin-test-query (pop requests))))
            (should (equal (cadr (assoc "startIndex" query)) (number-to-string (nth 1 case))))
            (should (equal (cadr (assoc "limit" query)) "7"))
            (should (equal (cadr (assoc "parentId" query)) "album-4"))
            (should (equal (cadr (assoc "searchTerm" query)) (nth 2 case)))))
        (elfin-items-search "new & term")
        (let ((query (elfin-test-query (pop requests))))
          (should (equal (cadr (assoc "startIndex" query)) "0"))
          (should (equal (cadr (assoc "searchTerm" query)) "new & term")))
        (setq elfin--current-page 1)
        (should-error (elfin-items-prev-page) :type 'user-error)
        (should-not requests)))))

(ert-deftest elfin-view-displays-each-type-and-missing-metadata ()
  (dolist (case '((albums [" " "Song" "Album artist" "1:01"]
                          [" " "Untitled" "Unknown" ""])
                  (artists [" " "Song" "3" "Jazz, Folk"]
                           [" " "Untitled" "0" ""])
                  (tracks [" " "Song" "First artist" "Record" "1:01"]
                          [" " "Untitled" "Unknown" "Unknown" ""])
                  (playlists [" " "Song" "3" "1:01"]
                             [" " "Untitled" "0" ""])))
    (with-temp-buffer
      (let ((buffer (current-buffer)) (elfin-thumbnail-size '(80 . 45)) callbacks)
        (cl-letf (((symbol-function 'elfin-view-buffer-name) (lambda (_) (buffer-name buffer)))
                  ((symbol-function 'switch-to-buffer) #'ignore)
                  ((symbol-function 'elfin--ensure-image-file)
                   (lambda (_item _width _height _ready error)
                     (push error callbacks))))
          (elfin--display-items
           (json-parse-string
            "[{\"Id\":\"a\",\"Name\":\"Song\",\"AlbumArtist\":\"Album artist\",\"Artists\":[\"First artist\",\"Second artist\"],\"Album\":\"Record\",\"RunTimeTicks\":610000000,\"ChildCount\":3,\"Genres\":[\"Jazz\",\"Folk\"]},{\"Id\":\"b\"}]")
           (car case) 2 "parent" nil)
          (should (derived-mode-p 'elfin-items-mode))
          (should (eq elfin--view-type (car case)))
          (should (= elfin--current-page 2))
          (should (equal elfin--view-parent-id "parent"))
          (should (equal tabulated-list-entries (list (list "a" (nth 1 case)) (list "b" (nth 2 case)))))
          (should (string-match-p "Song" (buffer-string)))
          (should (string-match-p "Untitled" (buffer-string)))
          (should buffer-read-only)
          ;; Missing artwork keeps the space reserved while loading.
          (dolist (callback callbacks) (funcall callback nil 'no-image))
          (should-not (string-match-p "\\[\\[" (buffer-string)))
          (let ((position (text-property-not-all (point-min) (point-max) 'display nil)))
            (should position)
            (should (equal (get-text-property position 'display)
                           '(space :width (0) :height (45) :ascent 50)))))))))

(ert-deftest elfin-view-artwork-keeps-geometry-through-loading-and-sorting ()
  (skip-unless (and (display-graphic-p) (image-type-available-p 'pbm)))
  (let ((directory (make-temp-file "elfin-artwork-" t))
        (elfin-thumbnail-size '(80 . 45))
        ;; Portrait, landscape, and a small image that must be enlarged.
        (cases '(("Portrait" 40 80 (23 . 45))
                 ("Landscape" 80 40 (80 . 40))
                 ("Tiny" 8 8 (45 . 45))
                 ("Missing" nil nil nil)))
        callbacks items)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-temp-buffer
            (let ((buffer (current-buffer)))
              (cl-loop for (name width height _size) in cases for n from 1
                       do (let ((item (make-hash-table :test #'equal)))
                            (puthash "Id" (format "%032d" n) item)
                            (puthash "Name" name item)
                            (push item items))
                       when width
                       do (with-temp-file (expand-file-name name directory)
                            (set-buffer-multibyte nil)
                            (insert (format "P6\n%d %d\n255\n" width height)
                                    (make-string (* width height 3) 80))))
              (cl-letf (((symbol-function 'elfin-view-buffer-name)
                         (lambda (_) (buffer-name buffer)))
                        ((symbol-function 'elfin--ensure-image-file)
                         (lambda (item _width _height ready error)
                           (push (list (gethash "Name" item) ready error) callbacks))))
                (elfin--display-items (vconcat (nreverse items)) 'albums 1 nil nil))
              ;; Sort before downloads finish, then reprint after they finish.
              (setq tabulated-list-sort-key '("Name" . nil))
              (tabulated-list-print t)
              (dolist (stage '(loading loaded reprinted))
                (when (eq stage 'loaded)
                  (dolist (callback callbacks)
                    (if (equal (car callback) "Missing")
                        (funcall (nth 2 callback) nil 'no-image)
                      (funcall (nth 1 callback)
                               (expand-file-name (car callback) directory)))))
                (when (eq stage 'reprinted) (tabulated-list-print t))
                (goto-char (point-min))
                (redisplay t)
                (let (name-x)
                  (dotimes (row 4)
                    (let* ((entry (tabulated-list-get-entry))
                           (name (aref entry 1))
                           (expected (nth 3 (assoc name cases)))
                           (name-pos (save-excursion
                                       (search-forward name (line-end-position))
                                       (match-beginning 0)))
                           (x (car (posn-x-y (posn-at-point name-pos))))
                           (p (line-beginning-position)) image)
                      (should (= (car (window-line-height row)) 45))
                      (if name-x (should (= x name-x)) (setq name-x x))
                      (while (< p name-pos)
                        (let ((display (get-text-property p 'display)))
                          (when (eq (car-safe display) 'image) (setq image display)))
                        (setq p (next-single-property-change p 'display nil name-pos)))
                      (if (or (eq stage 'loading) (null expected))
                          (should-not image)
                        (should image)
                        ;; `image-size' includes margins.  Check the artwork's
                        ;; fitted size and its horizontally centered outer box.
                        (let* ((size (image-size image t))
                               (margin (image-property image :margin)))
                          (should (equal (cons (- (car size) (* 2 (car margin)))
                                               (cdr size))
                                         expected))
                          (should (<= (abs (- (car size) 80)) 1)))))
                    (forward-line 1)))))))
      (delete-directory directory t))))

(ert-deftest elfin-view-fetch-response-populates-search-buffer ()
  (elfin-test-with-http
    (let ((name "*Elfin Tracks* [search: elfin-test-unique]"))
      (unwind-protect
          (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
            (elfin--show-view 'tracks 3 "album" "elfin-test-unique")
            (elfin-test-respond (pop requests) "{\"Items\":[]}")
            (with-current-buffer name
              (should (eq elfin--view-type 'tracks))
              (should (equal elfin--view-parent-id "album"))
              (should (= elfin--current-page 3))
              (should (equal elfin--view-search-term "elfin-test-unique"))
              (should-not tabulated-list-entries)))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest elfin-view-actions-dispatch-track-and-collection-ids ()
  (elfin-test-with-http
    (elfin-test-with-player
      (with-temp-buffer
        (elfin-items-mode)
        (setq tabulated-list-format [("Name" 20 t)]
              tabulated-list-entries '(("track-9" ["Track"]))
              elfin--view-type 'tracks)
        (tabulated-list-print)
        (goto-char (point-min))
        (elfin-items-open)
        (should (equal elfin--queue '("track-9")))
        (elfin-items-queue)
        (should (equal elfin--queue '("track-9" "track-9")))
        (elfin-items-play)
        (should (equal elfin--queue '("track-9")))
        (dolist (type '(albums artists playlists))
          (setq elfin--view-type type)
          (elfin-items-open)
          (let ((query (elfin-test-query (pop requests))))
            (should (equal (cadr (assoc "parentId" query)) "track-9"))
            (should (equal (cadr (assoc "includeItemTypes" query))
                           (if (eq type 'artists) "MusicAlbum" "Audio"))))
          (elfin-items-queue)
          (elfin-test-respond (pop requests) "{\"Items\":[{\"Id\":\"child\"}]}")
          (should (equal (car (last elfin--queue)) "child"))
          (elfin-items-play)
          (elfin-test-respond (pop requests) "{\"Items\":[{\"Id\":\"child\"}]}")
          (should (equal elfin--queue '("child"))))))))

(ert-deftest elfin-view-late-thumbnail-callback-tolerates-killed-buffer ()
  (let ((buffer (generate-new-buffer " *elfin-dead-view*")))
    (kill-buffer buffer)
    (should-not (elfin--insert-thumbnail buffer "a" "/unused/image.jpg"))))

;;; elfin-view-test.el ends here
