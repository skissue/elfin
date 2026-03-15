;;; elfin-vars.el --- Customization variables -*- lexical-binding: t -*-

;;; Commentary:

;; Elfin options, loaded first to pacify the compiler.

;;; Code:

(defgroup elfin nil
  "Jellyfin music client and player."
  :group 'multimedia
  :prefix "elfin-")

(defcustom elfin-debug nil
  "When non-nil, enable verbose debug logging for various functionality."
  :type 'boolean)

;;; Authentication

(defgroup elfin-auth nil
  "Jellyfin authentication settings."
  :group 'elfin)

(defcustom elfin-session-file nil
  "File in which to persist Jellyfin sessions.
When non-nil, sessions are saved to and restored from this file."
  :type '(choice (const :tag "Don't persist" nil)
                 file))

;;; mpv

(defgroup elfin-mpv nil
  "Settings for the mpv process."
  :group 'elfin)

(defcustom elfin-mpv-extra-options nil
  "List of extra options to pass to the mpv process."
  :type '(repeat string))

;;; Playback

(defgroup elfin-playback nil
  "Playback settings."
  :group 'elfin)

(defcustom elfin-replaygain nil
  "ReplayGain mode passed to mpv.
nil means disabled, `track' normalizes per-track, `album' normalizes per-album."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Track" track)
                 (const :tag "Album" album)))

(defcustom elfin-default-volume 100
  "Default volume passed to mpv (0-100)."
  :type 'integer)

(defcustom elfin-volume-step 5
  "Default volume adjustment step for volume up/down commands."
  :type 'integer)

;;; View

(defgroup elfin-view nil
  "Item listing view settings."
  :group 'elfin)

(defcustom elfin-thumbnail-size '(64 . 64)
  "Thumbnail size (width . height)."
  :type '(cons integer integer))

(defcustom elfin-max-items-per-page 100
  "Maximum items to list per page."
  :type 'integer)

(provide 'elfin-vars)

;;; elfin-vars.el ends here
