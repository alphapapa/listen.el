;;; listen-vlc.el --- VLC support for Emacs Music Player                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'listen-lib)

;;;; Types

(cl-defstruct
    (listen-player-vlc
     (:include listen-player
               (command "vlc")
               (args '("-I" "rc"))
               (max-volume
                ;; VLC allows very large volume percentages to boost volume far beyond what would be
                ;; practical (but it's sometimes useful).  It's not clear what the actual maximum
                ;; percentage is, even from looking at VLC's source code, but testing shows that
                ;; values up to at least 800 seem to work.  The Qt GUI for VLC 3.0.20 allows up to
                ;; 200% (in the OSD text, while showing a max of 125% in the slider), so let's go
                ;; with that.
                200))))

;;;; Functions

(cl-defmethod listen--info ((player listen-player-vlc))
  (with-temp-buffer
    (save-excursion
      (insert (listen--send player "info")))
    (cl-loop while (re-search-forward (rx bol "| " (group (1+ (not blank))) ": "
                                          (group (1+ (not (any ""))))) nil t)
             collect (cons (match-string 1) (match-string 2)))))

(cl-defmethod listen--filename ((player listen-player-vlc))
  "Return filename of PLAYER's current track."
  (let ((status (listen--send player "status")))
    (when (string-match (rx bol "( new input: file://" (group (1+ nonl)) " )" ) status)
      (match-string 1 status))))

(cl-defmethod listen--title ((player listen-player-vlc))
  (listen--send player "get_title"))

(cl-defmethod listen--ensure ((player listen-player-vlc))
  "Ensure PLAYER is ready."
  (pcase-let (((cl-struct listen-player command args process) player))
    (unless (process-live-p process)
      (setf (listen-player-process player)
            (apply #'start-process "listen-player-vlc" (generate-new-buffer " *listen-player-vlc*")
                   command args))
      (set-process-query-on-exit-flag (listen-player-process player) nil))))

(cl-defmethod listen--play ((player listen-player-vlc) file)
  "Play FILE with PLAYER.
Stops playing, clears playlist, adds FILE, and plays it."
  (dolist (command `("stop" "clear" ,(format "add %s" (expand-file-name file)) "play"))
    (listen--send player command)))

;; (cl-defmethod listen--stop ((player listen-player-vlc))
;;   "Stop playing with PLAYER."
;;   (listen--send player "stop"))

(cl-defmethod listen--status ((player listen-player-vlc))
  (let ((status (listen--send player "status")))
    (when (string-match (rx "( state " (group (1+ alnum)) " )") status)
      (match-string 1 status))))

(cl-defmethod listen--pause ((player listen-player-vlc))
  "Pause playing with PLAYER."
  (listen--send player "pause"))

(cl-defmethod listen--playing-p ((player listen-player-vlc))
  "Return non-nil if PLAYER is playing."
  (equal "1" (listen--send player "is_playing")))

(cl-defmethod listen--elapsed ((player listen-player-vlc))
  "Return seconds elapsed for PLAYER's track."
  (string-to-number (listen--send player "get_time")))

(cl-defmethod listen--length ((player listen-player-vlc))
  "Return length of PLAYER's track in seconds."
  (string-to-number (listen--send player "get_length")))

(cl-defmethod listen--send ((player listen-player-vlc) command)
  "Send COMMAND to PLAYER and return output."
  (listen--ensure player)
  (pcase-let (((cl-struct listen-player process) player))
    (with-current-buffer (process-buffer process)
      (let ((pos (marker-position (process-mark process))))
        (process-send-string process command)
        (process-send-string process "\n")
        (with-local-quit
          (accept-process-output process))
        (prog1 (buffer-substring pos (max (point-min) (- (process-mark process) 4)))
          (unless listen-debug-p
            (erase-buffer)))))))

(cl-defmethod listen--seek ((player listen-player-vlc) seconds)
  "Seek PLAYER to SECONDS."
  (listen--send player (format "seek %s" seconds)))

(cl-defmethod listen--volume ((player listen-player-vlc) &optional volume)
  "Return or set PLAYER's VOLUME.
VOLUME is an integer percentage."
  ;; While it is unclear from VLC's documentation, and even its source code at some revisions,
  ;; testing shows that the "rc" interface handles volume on a scale of 256 steps, where 255 = 100%
  ;; (and values >255 are >100%).  See <https://code.videolan.org/videolan/vlc/-/issues/25143> and
  ;; <https://code.videolan.org/videolan/vlc/-/commits/80b8c8254cb2fddd59d31ba3a46a6640d7ef23da>.
  (pcase-let (((cl-struct listen-player max-volume) player))
    (if volume
        (progn
          (unless (<= 0 volume max-volume)
            (user-error "VOLUME must be 0-%s" max-volume))
          (listen--send player (format "volume %s" (* 255 (/ volume 100.0)))))
      (* 100 (/ (string-to-number (listen--send player "volume")) 255.0)))))

(provide 'listen-vlc)

;;; listen-vlc.el ends here
