;;; jellyjam-scrobble.el --- ListenBrainz scrobbling -*- lexical-binding: t -*-

;;; Commentary:

;; ListenBrainz scrobbling integration for Jellyjam.

;;; Code:

(require 'plz)
(require 'jellyjam-api)
(require 'jellyjam-mpv)
(require 'jellyjam-playback)

(defgroup jellyjam-scrobble nil
  "ListenBrainz scrobbling settings."
  :group 'jellyjam)

(defcustom jellyjam-scrobble-enabled nil
  "Whether to scrobble tracks to ListenBrainz."
  :type 'boolean
  :group 'jellyjam-scrobble)

(defcustom jellyjam-listenbrainz-token nil
  "ListenBrainz user authentication token."
  :type '(choice (const nil) string)
  :group 'jellyjam-scrobble)

(defcustom jellyjam-listenbrainz-url "https://api.listenbrainz.org"
  "ListenBrainz API URL."
  :type 'string
  :group 'jellyjam-scrobble)

(defvar jellyjam--current-track nil
  "Hash table of current track metadata from Jellyfin.")

(defvar jellyjam--track-start-time nil
  "Unix timestamp when current track started playing.")

(defvar jellyjam--scrobbled-p nil
  "Whether current track has already been scrobbled.")

(defvar jellyjam--scrobble-observer-registered nil
  "Non-nil if time-pos observer has been registered for scrobbling.")

(defun jellyjam--listenbrainz-submit (listen-type payload)
  "Submit PAYLOAD to ListenBrainz with LISTEN-TYPE."
  (when (and jellyjam-scrobble-enabled jellyjam-listenbrainz-token)
    (let ((url (concat jellyjam-listenbrainz-url "/1/submit-listens"))
          (body (json-serialize
                 `(:listen_type ,listen-type
                                :payload ,(vector payload)))))
      (plz 'post url
        :headers `(("Authorization" . ,(concat "Token " jellyjam-listenbrainz-token))
                   ("Content-Type" . "application/json"))
        :body body
        :as #'json-parse-buffer
        :then (lambda (_response)
                (when (eq listen-type 'single)
                  (message "Scrobbled to ListenBrainz")))
        :else (lambda (err)
                (message "ListenBrainz submit failed: %S" err))))))

(defun jellyjam--build-listen-payload (track &optional listened-at)
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
                                          :submission_client "Jellyjam"
                                          :submission_client_version ,jellyjam--client-version)))
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

(defun jellyjam--scrobble-check-condition (time-pos)
  "Check if scrobble condition is met at TIME-POS seconds."
  (when (and jellyjam-scrobble-enabled
             jellyjam--current-track
             (not jellyjam--scrobbled-p))
    (let* ((runtime-ticks (gethash "RunTimeTicks" jellyjam--current-track))
           (duration-secs (and runtime-ticks (/ runtime-ticks 10000000.0)))
           (threshold (if duration-secs
                          (min (* duration-secs 0.5) 240)
                        240)))
      (when (>= time-pos threshold)
        (setq jellyjam--scrobbled-p t)
        (jellyjam--listenbrainz-submit
         "single"
         (jellyjam--build-listen-payload
          jellyjam--current-track jellyjam--track-start-time))))))

(defun jellyjam--scrobble-on-start-file (track-id)
  "Handle file start for scrobbling.
TRACK-ID is the Jellyfin item ID of the track."
  (when jellyjam-scrobble-enabled
    (jellyjam--get (format "/Items/%s" track-id) nil
      (setq jellyjam--current-track response)
      (setq jellyjam--track-start-time (floor (float-time)))
      (setq jellyjam--scrobbled-p nil)
      (jellyjam--listenbrainz-submit
       "playing_now"
       (jellyjam--build-listen-payload response)))))

(defun jellyjam--scrobble-on-end-file (_track-id _reason)
  "Handle file end for scrobbling."
  (setq jellyjam--current-track nil)
  (setq jellyjam--track-start-time nil)
  (setq jellyjam--scrobbled-p nil))

(defun jellyjam-scrobble-setup ()
  "Set up scrobbling hooks and observers."
  (add-hook 'jellyjam-file-start-hook #'jellyjam--scrobble-on-start-file)
  (add-hook 'jellyjam-file-end-hook #'jellyjam--scrobble-on-end-file)
  (unless jellyjam--scrobble-observer-registered
    (jellyjam-observe-property "time-pos" #'jellyjam--scrobble-check-condition)
    (setq jellyjam--scrobble-observer-registered t))
  (setq jellyjam-scrobble-enabled t))

(provide 'jellyjam-scrobble)

;;; jellyjam-scrobble.el ends here
