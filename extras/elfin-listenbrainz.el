;;; elfin-listenbrainz.el --- ListenBrainz scrobbling -*- lexical-binding: t -*-

;;; Commentary:

;; ListenBrainz scrobbling integration for Elfin.

;;; Code:

(require 'plz)
(require 'elfin)

(defgroup elfin-listenbrainz nil
  "ListenBrainz scrobbling settings."
  :group 'elfin)

(defcustom elfin-listenbrainz-token nil
  "ListenBrainz user authentication token."
  :type '(choice (const nil) string)
  :group 'elfin-listenbrainz)

(defcustom elfin-listenbrainz-url "https://api.listenbrainz.org"
  "ListenBrainz API URL."
  :type 'string
  :group 'elfin-listenbrainz)

(defvar elfin-listenbrainz-mode) ; forward declaration for byte-compiler

(defvar elfin--listenbrainz-current-track nil
  "Hash table of current track metadata from Jellyfin.")

(defvar elfin--listenbrainz-track-start-time nil
  "Unix timestamp when current track started playing.")

(defvar elfin--listenbrainz-scrobbled-p nil
  "Whether current track has already been scrobbled.")

(defun elfin--listenbrainz-submit (listen-type payload)
  "Submit PAYLOAD to ListenBrainz with LISTEN-TYPE."
  (when (and elfin-listenbrainz-mode elfin-listenbrainz-token)
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

(defun elfin--listenbrainz-build-payload (track &optional listened-at)
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
      (when-let* ((mbid (gethash "MusicBrainzTrack" provider-ids)))
        (setq additional-info (plist-put additional-info :recording_mbid mbid)))
      (when-let* ((mbid (gethash "MusicBrainzAlbum" provider-ids)))
        (setq additional-info (plist-put additional-info :release_mbid mbid)))
      (when-let* ((mbid (gethash "MusicBrainzArtist" provider-ids)))
        (setq additional-info (plist-put additional-info :artist_mbids (vector mbid)))))
    (let ((payload `(:track_metadata
                     (:artist_name ,artist-name
                                   :track_name ,track-name
                                   :release_name ,release-name
                                   :additional_info ,additional-info))))
      (when listened-at
        (setq payload (plist-put payload :listened_at listened-at)))
      payload)))

(defun elfin--listenbrainz-check-condition (time-pos)
  "Check if scrobble condition is met at TIME-POS seconds."
  (when (and elfin-listenbrainz-mode
             time-pos
             elfin--listenbrainz-current-track
             (not elfin--listenbrainz-scrobbled-p))
    (let* ((runtime-ticks (gethash "RunTimeTicks" elfin--listenbrainz-current-track))
           (duration-secs (and runtime-ticks (/ runtime-ticks 10000000.0)))
           (threshold (if duration-secs
                          (min (* duration-secs 0.5) 240)
                        240)))
      (when (>= time-pos threshold)
        (setq elfin--listenbrainz-scrobbled-p t)
        (elfin--listenbrainz-submit
         "single"
         (elfin--listenbrainz-build-payload
          elfin--listenbrainz-current-track elfin--listenbrainz-track-start-time))))))

(defun elfin--listenbrainz-on-start-file (track-id)
  "Handle file start for scrobbling.
TRACK-ID is the Jellyfin item ID of the track."
  (when elfin-listenbrainz-mode
    (elfin--get (format "/Items/%s" track-id) nil
      (setq elfin--listenbrainz-current-track response)
      (setq elfin--listenbrainz-track-start-time (floor (float-time)))
      (setq elfin--listenbrainz-scrobbled-p nil)
      (elfin--listenbrainz-submit
       "playing_now"
       (elfin--listenbrainz-build-payload response)))))

(defun elfin--listenbrainz-on-end-file (_track-id _reason)
  "Handle file end for scrobbling."
  (setq elfin--listenbrainz-current-track nil)
  (setq elfin--listenbrainz-track-start-time nil)
  (setq elfin--listenbrainz-scrobbled-p nil))

;;;###autoload
(define-minor-mode elfin-listenbrainz-mode
  "Global minor mode for scrobbling tracks to ListenBrainz."
  :global t
  :group 'elfin-listenbrainz
  (if elfin-listenbrainz-mode
      (progn
        (unless elfin-listenbrainz-token
          (warn "`elfin-listenbrainz-mode' enabled without `elfin-listenbrainz-token' set"))
        (add-hook 'elfin-file-start-hook #'elfin--listenbrainz-on-start-file)
        (add-hook 'elfin-file-end-hook #'elfin--listenbrainz-on-end-file)
        (elfin-observe-property "time-pos" #'elfin--listenbrainz-check-condition))
    (remove-hook 'elfin-file-start-hook #'elfin--listenbrainz-on-start-file)
    (remove-hook 'elfin-file-end-hook #'elfin--listenbrainz-on-end-file)
    (elfin-unobserve-property "time-pos" #'elfin--listenbrainz-check-condition)
    (setq elfin--listenbrainz-current-track nil
          elfin--listenbrainz-track-start-time nil
          elfin--listenbrainz-scrobbled-p nil)))

(provide 'elfin-listenbrainz)

;;; elfin-listenbrainz.el ends here
