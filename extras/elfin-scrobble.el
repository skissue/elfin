;;; elfin-scrobble.el --- ListenBrainz scrobbling -*- lexical-binding: t -*-

;;; Commentary:

;; ListenBrainz scrobbling integration for Elfin.

;;; Code:

(require 'plz)
(require 'elfin)

(defgroup elfin-scrobble nil
  "ListenBrainz scrobbling settings."
  :group 'elfin)

(defcustom elfin-listenbrainz-token nil
  "ListenBrainz user authentication token."
  :type '(choice (const nil) string)
  :group 'elfin-scrobble)

(defcustom elfin-listenbrainz-url "https://api.listenbrainz.org"
  "ListenBrainz API URL."
  :type 'string
  :group 'elfin-scrobble)

(defvar elfin--current-track nil
  "Hash table of current track metadata from Jellyfin.")

(defvar elfin--track-start-time nil
  "Unix timestamp when current track started playing.")

(defvar elfin--scrobbled-p nil
  "Whether current track has already been scrobbled.")

(defun elfin--listenbrainz-submit (listen-type payload)
  "Submit PAYLOAD to ListenBrainz with LISTEN-TYPE."
  (when (and elfin-scrobble-mode elfin-listenbrainz-token)
    (let ((url (concat elfin-listenbrainz-url "/1/submit-listens"))
          (body (json-serialize
                 `(:listen_type ,listen-type
                                :payload ,(vector payload)))))
      (plz 'post url
        :headers `(("Authorization" . ,(concat "Token " elfin-listenbrainz-token))
                   ("Content-Type" . "application/json"))
        :body body
        :as #'json-parse-buffer
        :then (lambda (_response)
                (when (eq listen-type 'single)
                  (message "Scrobbled to ListenBrainz")))
        :else (lambda (err)
                (message "ListenBrainz submit failed: %S" err))))))

(defun elfin--build-listen-payload (track &optional listened-at)
  "Build ListenBrainz payload from Jellyfin TRACK metadata.
If LISTENED-AT is provided, include it for scrobble submissions."
  (let* ((artists (gethash "Artists" track))
         (artist-name (or (and artists (> (length artists) 0) (aref artists 0))
                          (gethash "AlbumArtist" track)))
         (track-name (gethash "Name" track))
         (release-name (gethash "Album" track))
         (runtime-ticks (gethash "RunTimeTicks" track))
         (index-number (gethash "IndexNumber" track))
         (provider-ids (gethash "ProviderIds" track))
         (additional-info `(:media_player "Emacs"
                                          :submission_client "Elfin"
                                          :submission_client_version ,elfin--client-version)))
    (when runtime-ticks
      (setq additional-info
            (plist-put additional-info :duration_ms (/ runtime-ticks 10000))))
    (when index-number
      (setq additional-info
            (plist-put additional-info :tracknumber index-number)))
    (when provider-ids
      (when-let ((mbid (gethash "MusicBrainzTrack" provider-ids)))
        (setq additional-info (plist-put additional-info :recording_mbid mbid)))
      (when-let ((mbid (gethash "MusicBrainzAlbum" provider-ids)))
        (setq additional-info (plist-put additional-info :release_mbid mbid)))
      (when-let ((mbid (gethash "MusicBrainzArtist" provider-ids)))
        (setq additional-info (plist-put additional-info :artist_mbids (vector mbid)))))
    (let ((payload `(:track_metadata
                     (:artist_name ,artist-name
                                   :track_name ,track-name
                                   :release_name ,release-name
                                   :additional_info ,additional-info))))
      (when listened-at
        (setq payload (plist-put payload :listened_at listened-at)))
      payload)))

(defun elfin--scrobble-check-condition (time-pos)
  "Check if scrobble condition is met at TIME-POS seconds."
  (when (and elfin-scrobble-mode
             time-pos
             elfin--current-track
             (not elfin--scrobbled-p))
    (let* ((runtime-ticks (gethash "RunTimeTicks" elfin--current-track))
           (duration-secs (and runtime-ticks (/ runtime-ticks 10000000.0)))
           (threshold (if duration-secs
                          (min (* duration-secs 0.5) 240)
                        240)))
      (when (>= time-pos threshold)
        (setq elfin--scrobbled-p t)
        (elfin--listenbrainz-submit
         "single"
         (elfin--build-listen-payload
          elfin--current-track elfin--track-start-time))))))

(defun elfin--scrobble-on-start-file (track-id)
  "Handle file start for scrobbling.
TRACK-ID is the Jellyfin item ID of the track."
  (when elfin-scrobble-mode
    (elfin--get (format "/Items/%s" track-id) nil
      (setq elfin--current-track response)
      (setq elfin--track-start-time (floor (float-time)))
      (setq elfin--scrobbled-p nil)
      (elfin--listenbrainz-submit
       "playing_now"
       (elfin--build-listen-payload response)))))

(defun elfin--scrobble-on-end-file (_track-id _reason)
  "Handle file end for scrobbling."
  (setq elfin--current-track nil)
  (setq elfin--track-start-time nil)
  (setq elfin--scrobbled-p nil))

;;;###autoload
(define-minor-mode elfin-scrobble-mode
  "Global minor mode for scrobbling tracks to ListenBrainz."
  :global t
  :group 'elfin-scrobble
  (if elfin-scrobble-mode
      (progn
        (unless elfin-listenbrainz-token
          (warn "`elfin-scrobble-mode' enabled without `elfin-listenbrainz-token' set"))
        (add-hook 'elfin-file-start-hook #'elfin--scrobble-on-start-file)
        (add-hook 'elfin-file-end-hook #'elfin--scrobble-on-end-file)
        (elfin-observe-property "time-pos" #'elfin--scrobble-check-condition))
    (remove-hook 'elfin-file-start-hook #'elfin--scrobble-on-start-file)
    (remove-hook 'elfin-file-end-hook #'elfin--scrobble-on-end-file)
    (elfin-unobserve-property "time-pos" #'elfin--scrobble-check-condition)
    (setq elfin--current-track nil
          elfin--track-start-time nil
          elfin--scrobbled-p nil)))

(provide 'elfin-scrobble)

;;; elfin-scrobble.el ends here
