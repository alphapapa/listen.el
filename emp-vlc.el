;;; emp-vlc.el --- VLC support for Emacs Music Player                    -*- lexical-binding: t; -*-

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

;; (require 'emp)

;;;; Types

(cl-defstruct (emp-player-vlc
               (:include emp-player
                         (command "vlc")
                         (args '("-I" "rc")))))

;;;; Functions

(cl-defmethod emp-info ((player emp-player-vlc))
  (emp-send player "info"))

(cl-defmethod emp-title ((player emp-player-vlc))
  (emp-send player "get_title"))

(cl-defmethod emp-ensure ((player emp-player-vlc))
  "Ensure PLAYER is ready."
  (pcase-let (((cl-struct emp-player command args process) player))
    (unless (and process (process-live-p process))
      (setf (emp-player-process player)
            (apply #'start-process "emp-player-vlc" (get-buffer-create " *emp-player-vlc*")
                   command args)))))

(cl-defmethod emp--play ((player emp-player-vlc) file)
  "Play FILE with PLAYER."
  (dolist (command `("clear" ,(format "add %s" (expand-file-name file)) "play"))
    (emp-send player command)))

(cl-defmethod emp--stop ((player emp-player-vlc))
  "Stop playing with PLAYER."
  (emp-send player "stop"))

(cl-defmethod emp--status ((player emp-player-vlc))
  (let ((status (emp-send player "status")))
    (when (string-match (rx "( state " (group (1+ alnum)) " )") status)
      (match-string 1 status))))

(cl-defmethod emp--pause ((player emp-player-vlc))
  "Pause playing with PLAYER."
  (emp-send player "pause"))

(cl-defmethod emp-playing-p ((player emp-player-vlc))
  "Return non-nil if PLAYER is playing."
  (equal "1" (emp-send player "is_playing")))

(cl-defmethod emp-elapsed ((player emp-player-vlc))
  "Return seconds elapsed for PLAYER's track."
  (cl-parse-integer (emp-send player "get_time")))

(cl-defmethod emp-length ((player emp-player-vlc))
  "Return length of  PLAYER's track."
  (cl-parse-integer (emp-send player "get_length")))

(cl-defmethod emp-send ((player emp-player-vlc) command)
  "Send COMMAND to PLAYER and return output."
  (emp-ensure player)
  (pcase-let (((cl-struct emp-player process) player))
    (with-current-buffer (process-buffer process)
      (let ((pos (marker-position (process-mark process))))
        (process-send-string process command)
        (process-send-string process "\n")
        (with-local-quit
          (accept-process-output process))
        (buffer-substring pos (- (process-mark process) 4))))))

(provide 'emp-vlc)

;;; emp-vlc.el ends here
