;;; run-tests.el --- Source-only offline ERT runner -*- lexical-binding: t -*-

(require 'cl-lib)
(let* ((tests (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name tests))))
  (add-to-list 'load-path root)
  (add-to-list 'load-path (expand-file-name "extras" root))
  (add-to-list 'load-path tests)
  ;; Explicit filenames also defeat bytecode with newer timestamps.
  (dolist (file '("elfin-vars.el" "elfin-api.el" "elfin-auth.el"
                  "elfin-mpv.el" "elfin-playback.el" "elfin-image.el"
                  "elfin-view.el" "elfin.el" "extras/elfin-listenbrainz.el"))
    (load-file (expand-file-name file root)))
  (load-file (expand-file-name "elfin-test-helper.el" tests))
  (dolist (file (directory-files tests t "-test\\.el\\'"))
    (load-file file)))

;; A forgotten mock must fail instead of contacting a server, spawning mpv,
;; or connecting to an existing player.  Individual tests override these guards.
(cl-letf (((symbol-function 'plz)
           (lambda (&rest _) (ert-fail "Unmocked HTTP request")))
          ((symbol-function 'plz-run)
           (lambda (&rest _) (ert-fail "Unmocked HTTP queue")))
          ((symbol-function 'make-process)
           (lambda (&rest _) (ert-fail "Unmocked subprocess")))
          ((symbol-function 'make-network-process)
           (lambda (&rest _) (ert-fail "Unmocked network connection"))))
  (ert-run-tests-batch-and-exit t))

;;; run-tests.el ends here
