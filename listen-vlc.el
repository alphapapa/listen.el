;;; listen-vlc.el --- VLC support for Emacs Music Player                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Adam Porter

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
  (listen--send player "info"))

(cl-defmethod listen--title ((player listen-player-vlc))
  (listen--send player "get_title"))

(cl-defmethod listen--ensure ((player listen-player-vlc))
  "Ensure PLAYER is ready."
  (pcase-let (((cl-struct listen-player command args process) player))
    (unless (and process (process-live-p process))
      (setf (listen-player-process player)
            (apply #'start-process "listen-player-vlc" (generate-new-buffer " *listen-player-vlc*")
                   command args)))))

(cl-defmethod listen--play ((player listen-player-vlc) file)
  "Play FILE with PLAYER."
  (dolist (command `("stop" "clear" ,(format "add %s" (expand-file-name file)) "play"))
    (listen--send player command)))

(cl-defmethod listen--stop ((player listen-player-vlc))
  "Stop playing with PLAYER."
  (listen--send player "stop"))

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
  (cl-parse-integer (listen--send player "get_time")))

(cl-defmethod listen--length ((player listen-player-vlc))
  "Return length of  PLAYER's track."
  (cl-parse-integer (listen--send player "get_length")))

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

(cl-defmethod listen--volume ((player listen-player-vlc) &optional volume)
  "Return or set PLAYER's VOLUME.
VOLUME is an integer percentage."
  (if volume
      (listen--send player (format "volume %s" (* 255 (/ volume 100.0))))
    (* 100 (/ (cl-parse-integer (listen--send player "volume")) 255.0))))

(provide 'listen-vlc)

;;; listen-vlc.el ends here
