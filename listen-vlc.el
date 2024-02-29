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

(cl-defstruct (listen-player-vlc
               (:include listen-player
                         (command "vlc")
                         (args '("-I" "rc")))))

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
  (if volume
      (progn
        (cl-assert (<= 0 volume 100) nil "VOLUME must be 0-100")
        (listen--send player (format "volume %s" (* 255 (/ volume 100.0)))))
    (* 100 (/ (string-to-number (listen--send player "volume")) 255.0))))

(provide 'listen-vlc)

;;; listen-vlc.el ends here
