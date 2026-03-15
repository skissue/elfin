;;; elfin.el --- Jellyfin music player -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (plz "0.9.1"))
;; Homepage: https://github.com/skissue/elfin.el
;; Keywords: multimedia


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Jellyfin music client and player utilizing mpv.

;;; Code:

(require 'elfin-auth)
(require 'elfin-view)
(require 'elfin-mpv)
(require 'elfin-playback)

(provide 'elfin)

;;; elfin.el ends here
