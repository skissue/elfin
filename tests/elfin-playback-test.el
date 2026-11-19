;;; elfin-playback-test.el --- Playback commands and hooks -*- lexical-binding: t -*-

(require 'elfin-test-helper)

(ert-deftest elfin-playback-play-and-append-preserve-queue-and-entry-identity ()
  (elfin-test-with-http
    (elfin-test-with-player
      (elfin-play-track "z" t)
      (elfin-queue-tracks '("a" "z") t)
      (should (equal elfin--queue '("z" "a" "z")))
      (should (= (elfin-queue-length) 3))
      (should (= elfin--queue-pos 0))
      (let ((loads (seq-filter (lambda (command) (equal (caar command) "loadfile"))
                               (reverse commands))))
        (should (equal (mapcar (lambda (command) (nth 2 (car command))) loads)
                       '("replace" "append" "append")))
        (should (equal (mapcar (lambda (command) (nth 1 (car command))) loads)
                       (mapcar #'elfin--audio-url '("z" "a" "z"))))
        ;; Deliver in reverse order to catch a callback capturing the wrong ID.
        (cl-loop for command in (reverse loads) for entry-id from 10
                 do (funcall (cdr command)
                             (json-parse-string (format "{\"playlist_entry_id\":%d}" entry-id))))
        (should (equal (gethash 10 elfin--entry-map) "z"))
        (should (equal (gethash 11 elfin--entry-map) "a"))
        (should (equal (gethash 12 elfin--entry-map) "z")))
      (should (assoc '("set_property" "pause" :false) commands))
      (elfin-play-track "replacement" t)
      (should (equal elfin--queue '("replacement")))
      ;; Replacing the playlist must not lose the outgoing file's identity.
      (let* (ended
             (elfin-file-end-hook (list (lambda (id _reason) (setq ended id)))))
        (elfin--handle-end-file
         (json-parse-string "{\"playlist_entry_id\":12,\"reason\":\"stop\"}"))
        (should (equal ended "z"))))))

(ert-deftest elfin-playback-collections-replace-or-append-after-response ()
  (elfin-test-with-http
    (elfin-test-with-player
      (setq elfin--queue '("old") elfin--queue-pos 0)
      (elfin-play-collection "album")
      (should (equal elfin--queue '("old")))
      (elfin-test-respond (pop requests) "{\"Items\":[{\"Id\":\"b\"},{\"Id\":\"a\"}]}")
      (should (equal elfin--queue '("b" "a")))
      (should (assoc '("playlist-remove" "current") commands))
      (elfin-queue-collection "playlist")
      (elfin-test-respond (pop requests) "{\"Items\":[{\"Id\":\"c\"}]}")
      (should (equal elfin--queue '("b" "a" "c")))
      (elfin-play-collection "empty")
      (elfin-test-respond (pop requests) "{\"Items\":[]}")
      (should (equal elfin--queue '("b" "a" "c"))))))

(ert-deftest elfin-playback-stop-and-clear-reset-queue ()
  (dolist (command '(elfin-stop elfin-clear-queue))
    (elfin-test-with-player
      (setq elfin--queue '("a" "b") elfin--queue-pos 1)
      (puthash 9 "b" elfin--entry-map)
      (funcall command)
      (should (= (elfin-queue-length) 0))
      (should-not elfin--queue-pos)
      (let* (ended
             (elfin-file-end-hook (list (lambda (id _reason) (setq ended id)))))
        (elfin--handle-end-file
         (json-parse-string "{\"playlist_entry_id\":9,\"reason\":\"stop\"}"))
        (should (equal ended "b")))
      (if (eq command 'elfin-stop)
          (should (assoc '("stop") commands))
        (should (assoc '("playlist-clear") commands))
        (should (assoc '("playlist-remove" "current") commands))))))

(ert-deftest elfin-playback-volume-clamps-both-boundaries ()
  (elfin-test-with-player
    (let ((elfin-volume-step 7))
      (dolist (case '((elfin-volume-up 96 100) (elfin-volume-up 32 39)
                      (elfin-volume-down 3 0) (elfin-volume-down 32 25)))
        (setq commands nil)
        (funcall (nth 0 case))
        (should (equal (caar commands) '("get_property" "volume")))
        (funcall (cdar commands) (nth 1 case))
        (should (equal (caar commands) (list "set_property" "volume" (nth 2 case)))))
      (setq commands nil)
      (elfin-volume-up)
      (funcall (cdar commands) nil)
      (should (= (length commands) 1)))))

(ert-deftest elfin-playback-pause-and-loop-toggles ()
  (elfin-test-with-player
    (elfin-pause)
    (should (equal (caar commands) '("cycle" "pause")))
    (dolist (case '((elfin-loop-track "loop-file")
                    (elfin-loop-playlist "loop-playlist")))
      (dolist (value '("no" "inf"))
        (funcall (car case))
        (should (equal (caar commands) (list "get_property" (cadr case))))
        (funcall (cdar commands) value)
        (should (equal (caar commands)
                       (list "set_property" (cadr case) (if (equal value "inf") "no" "inf"))))))))

(ert-deftest elfin-playback-ipc-events-deliver-hook-contracts ()
  (elfin-test-with-player
    (let* ((elfin--ipc-buffer "")
           (elfin--property-observers '((1 . "pause")))
           (elfin--property-callbacks '(("pause" elfin--handle-pause)))
           (elfin--event-callbacks
            '(("start-file" elfin--handle-start-file)
              ("end-file" elfin--handle-end-file)
              ("playback-restart" elfin--handle-playback-restart)
              ("idle" elfin--handle-idle)))
           starts ends pauses idle
           (elfin-file-start-hook (list (lambda (id) (push id starts))))
           (elfin-file-end-hook (list (lambda (id reason) (push (list id reason) ends))))
           (elfin-pause-hook (list (lambda (paused) (push paused pauses))))
           (elfin-idle-hook
            (list (lambda () (setq idle (list elfin--current-entry-id elfin-current-track-id))))))
      (puthash 17 "track-a" elfin--entry-map)
      (puthash 18 "track-b" elfin--entry-map)
      (elfin--ipc-filter nil "{\"event\":\"start-file\",\"playlist_entry_id\":17}\n")
      (should (equal elfin-current-track-id "track-a"))
      (elfin--ipc-filter nil "{\"event\":\"playback-restart\"}\n")
      (should (equal starts '("track-a" "track-a")))
      ;; An end for another entry must not clear the current track.
      (elfin--ipc-filter nil "{\"event\":\"end-file\",\"playlist_entry_id\":18,\"reason\":\"eof\"}\n")
      (should (equal ends '(("track-b" "eof"))))
      (should (equal elfin-current-track-id "track-a"))
      ;; Different entries can refer to the same Jellyfin track.
      (puthash 19 "track-a" elfin--entry-map)
      (elfin--ipc-filter nil "{\"event\":\"end-file\",\"playlist_entry_id\":19,\"reason\":\"stop\"}\n")
      (should (equal elfin-current-track-id "track-a"))
      (elfin--ipc-filter nil "{\"event\":\"end-file\",\"playlist_entry_id\":17,\"reason\":\"eof\"}\n")
      (should-not elfin-current-track-id)
      ;; The same entry can start again when the playlist loops.
      (elfin--ipc-filter nil "{\"event\":\"start-file\",\"playlist_entry_id\":17}\n")
      (should (equal (car starts) "track-a"))
      ;; Unknown or missing entry IDs cannot be attributed to track-a.
      (let (warnings)
        (cl-letf (((symbol-function 'warn) (lambda (&rest args) (push args warnings))))
          (elfin--ipc-filter nil "{\"event\":\"end-file\",\"playlist_entry_id\":99,\"reason\":\"stop\"}\n")
          (elfin--ipc-filter nil "{\"event\":\"end-file\",\"reason\":\"stop\"}\n")
          (should (equal elfin-current-track-id "track-a"))
          (should (equal ends '(("track-a" "eof") ("track-a" "stop") ("track-b" "eof"))))
          (elfin--ipc-filter nil "{\"event\":\"start-file\",\"playlist_entry_id\":99}\n")
          (should-not elfin-current-track-id)
          (elfin--ipc-filter nil "{\"event\":\"playback-restart\"}\n")
          (should (equal starts '("track-a" "track-a" "track-a")))
          (should (= (length warnings) 3))))
      ;; Idle clears stale identity even if the matching end event was missed.
      (elfin--ipc-filter nil "{\"event\":\"start-file\",\"playlist_entry_id\":17}\n")
      (elfin--ipc-filter nil (concat "{\"event\":\"property-change\",\"id\":1,\"data\":true}\n"
                                    "{\"event\":\"property-change\",\"id\":1,\"data\":false}\n"
                                    "{\"event\":\"idle\"}\n"))
      (should (equal (reverse pauses) '(t nil)))
      (should (equal idle '(nil nil)))
      (should-not elfin--current-entry-id)
      (should-not elfin-current-track-id)
      ;; Idle does not discard mappings for subsequent playback.
      (elfin--ipc-filter nil "{\"event\":\"start-file\",\"playlist_entry_id\":18}\n")
      (should (equal elfin-current-track-id "track-b"))
      (should (equal (car starts) "track-b")))))

;;; elfin-playback-test.el ends here
