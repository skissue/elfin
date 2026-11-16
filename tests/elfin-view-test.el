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
  (dolist (case '((albums ["[[a]]" "Song" "Album artist" "1:01"]
                          ["[[b]]" "Untitled" "Unknown" ""])
                  (artists ["[[a]]" "Song" "3" "Jazz, Folk"]
                           ["[[b]]" "Untitled" "0" ""])
                  (tracks ["[[a]]" "Song" "First artist" "Record" "1:01"]
                          ["[[b]]" "Untitled" "Unknown" "Unknown" ""])
                  (playlists ["[[a]]" "Song" "3" "1:01"]
                             ["[[b]]" "Untitled" "0" ""])))
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
          ;; Late image errors update the correct row, even in reverse order.
          (dolist (callback callbacks) (funcall callback nil 'no-image))
          (should-not (string-match-p "\\[\\[" (buffer-string)))
          (let ((position (text-property-not-all (point-min) (point-max) 'display nil)))
            (should position)
            (should (equal (get-text-property position 'display)
                           `(space :width ,(/ 80.0 (frame-char-width))
                                   :height ,(/ 45.0 (frame-char-height)))))))))))

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
    (should-not (elfin--insert-blank-thumbnail buffer "a"))
    (should-not (elfin--insert-thumbnail buffer "a" "/unused/image.jpg"))))

;;; elfin-view-test.el ends here
