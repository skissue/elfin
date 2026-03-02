;;; elfin-playback.el --- mpv playback control -*- lexical-binding: t -*-

;;; Commentary:

;; mpv-based audio playback for Jellyfin tracks.

;;; Code:

(require 'elfin-api)
(require 'elfin-mpv)

(defvar elfin--queue nil
  "List of track IDs in playback order.")

(defvar elfin--queue-pos nil
  "Current position in queue (0-indexed), or nil if empty.")

(defvar elfin--current-track-id nil
  "ID of the currently playing track, set on file-start.")

(defvar elfin-file-start-hook nil
  "Hook run when a new file starts playing.
Each function receives the track ID as its single argument.")

(defvar elfin-file-end-hook nil
  "Hook run when a file finishes playing.
Each function receives the track ID and reason as arguments.")

(defvar elfin-pause-hook nil
  "Hook run when pause state changes.
Each function receives non-nil if paused, nil if playing.")

(defvar elfin-idle-hook nil
  "Hook run when mpv enters idle state.
Functions receive no arguments.")

(defun elfin-current-track ()
  "Return the ID of the currently playing track, or nil."
  (when (and elfin--queue elfin--queue-pos)
    (nth elfin--queue-pos elfin--queue)))

(defun elfin-queue-length ()
  "Return the number of tracks in the queue."
  (length elfin--queue))

(defcustom elfin-default-volume 100
  "Default volume passed to mpv (0-100)."
  :type 'integer
  :group 'elfin)

(defcustom elfin-volume-step 5
  "Default volume adjustment step for volume up/down commands."
  :type 'integer
  :group 'elfin)

(defun elfin-play-track (id &optional silent)
  "Play track ID with mpv.
Show a message notifying the user unless SILENT is non-nil."
  (setq elfin--queue (list id))
  (setq elfin--queue-pos 0)
  (elfin--mpv-send "loadfile" (elfin--audio-url id) "replace")
  (elfin--mpv-send "set_property" "pause" :false)
  (unless silent
    (message "Playing track")))

(defun elfin-queue-track (id &optional silent)
  "Add track ID to mpv playlist.
Show a message notifying the user unless SILENT is non-nil."
  (setq elfin--queue (append elfin--queue (list id)))
  (elfin--mpv-send "loadfile" (elfin--audio-url id) "append")
  (unless silent
    (message "Queued track")))

(defun elfin-queue-tracks (ids &optional silent)
  "Queue multiple track IDS.
Show a message notifying the user unless SILENT is non-nil."
  (dolist (id ids)
    (elfin-queue-track id :silent))
  (unless silent
    (message "Queued %d tracks" (length ids))))

(defun elfin-clear-queue (&optional silent)
  "Clear the Elfin playback queue.
Show a message notifying the user unless SILENT is non-nil."
  (interactive)
  (setq elfin--queue nil)
  (setq elfin--queue-pos nil)
  (elfin--mpv-send "playlist-clear")
  ;; `playlist-clear' does not remove the current file.
  (elfin--mpv-send "playlist-remove" "current")
  (unless silent
    (message "Cleared queue")))

(defun elfin-play-collection (parent-id)
  "Clear queue and play all tracks under PARENT-ID."
  (elfin--get-child-items parent-id
      (elfin-clear-queue :silent)
    (elfin-play-track (car ids) :silent)
    (elfin-queue-tracks (cdr ids) :silent)
    (message "Playing %d tracks" (length ids))))

(defun elfin-queue-collection (parent-id)
  "Add all tracks under PARENT-ID to the queue."
  (elfin--get-child-items parent-id
      (elfin-queue-tracks ids)))

(defun elfin-stop ()
  "Stop Elfin playback."
  (interactive)
  (setq elfin--queue nil)
  (setq elfin--queue-pos nil)
  (elfin--mpv-send "stop"))

(defun elfin-pause ()
  "Toggle Elfin pause state."
  (interactive)
  (elfin--mpv-send "cycle" "pause"))

(defun elfin-volume-set (volume)
  "Set Elfin volume to VOLUME (0-100)."
  (interactive "nVolume: ")
  (elfin--mpv-send "set_property" "volume" volume)
  (message "Volume: %d" volume))

(defun elfin-volume-up ()
  "Increase Elfin volume."
  (interactive)
  (let* ((response (elfin--mpv-send "get_property" "volume"))
         (current (gethash "data" response)))
    (when current
      (elfin-volume-set (min 100 (+ current elfin-volume-step))))))

(defun elfin-volume-down ()
  "Decrease Elfin volume."
  (interactive)
  (let* ((response (elfin--mpv-send "get_property" "volume"))
         (current (gethash "data" response)))
    (when current
      (elfin-volume-set (max 0 (- current elfin-volume-step))))))

;; Wire up mpv events to hooks.
(elfin-observe-property "playlist-playing-pos"
                           (lambda (pos) (setq elfin--queue-pos pos)))

(elfin--add-event-handler "start-file"
                             (lambda (_event)
                               (let ((id (elfin-current-track)))
                                 (setq elfin--current-track-id id)
                                 (run-hook-with-args 'elfin-file-start-hook id))))

(elfin--add-event-handler "end-file"
                             (lambda (event)
                               (let ((id elfin--current-track-id)
                                     (reason (gethash "reason" event)))
                                 (run-hook-with-args 'elfin-file-end-hook id reason)
                                 (setq elfin--current-track-id nil))))

(elfin-observe-property "pause"
                           (lambda (paused-p)
                             (run-hook-with-args 'elfin-pause-hook paused-p)))

(elfin--add-event-handler "idle"
                             (lambda (_event)
                               (run-hooks 'elfin-idle-hook)))

(provide 'elfin-playback)

;;; elfin-playback.el ends here
