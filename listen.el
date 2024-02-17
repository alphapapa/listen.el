;;; listen.el --- Music player                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: 

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
(require 'listen-vlc)

;;;; Variables

(defvar listen-player nil)

(defvar listen-mode-update-mode-line-timer nil)

;;;; Customization

(defgroup listen nil
  "Music player."
  :group 'applications)

(defcustom listen-directory "~/Music"
  "Default music directory."
  :type 'directory)

;;;; Functions

(cl-defmethod listen--running-p ((player listen-player))
  "Return non-nil if PLAYER is running."
  (process-live-p (listen-player-process player)))

(defun listen--player ()
  "Return `listen-player' or a newly set one if nil."
  (or listen-player
      (setf listen-player (make-listen-player-vlc))))

;;;; Mode

(defvar listen-mode-lighter nil)

(define-minor-mode listen-mode
  "Show Listen player status in the mode line."
  :global t
  (let ((lighter '(listen-mode listen-mode-lighter)))
    (if listen-mode
        (progn
          (when (timerp listen-mode-update-mode-line-timer)
            ;; Cancel any existing timer.  Generally shouldn't happen, but not impossible.
            (cancel-timer listen-mode-update-mode-line-timer))
          (setf listen-mode-update-mode-line-timer (run-with-timer 1 1 #'listen--mode-line-update))
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew lighter global-mode-string :test #'equal))
      (when listen-mode-update-mode-line-timer
        (cancel-timer listen-mode-update-mode-line-timer)
        (setf listen-mode-update-mode-line-timer nil))
      (setf global-mode-string
            (remove lighter global-mode-string)))))

(defcustom listen-lighter-format 'remaining
  "Time elapsed/remaining format."
  :type '(choice (const remaining)
                 (const elapsed)))

(defun listen-mode-lighter ()
  "Return lighter for `listen-mode'."
  (cl-labels ((format-time (seconds)
                (format-seconds "%h:%.2m:%.2s%z" seconds))
              (format-track ()
                (listen--title listen-player))
              (format-status ()
                (pcase (listen--status listen-player)
                  ("playing" "▶")
                  ("paused" "⏸")
                  ("stopped" "■"))))
    (apply #'concat "🎵:"
           (if (and (listen--running-p listen-player)
                    (listen--playing-p listen-player))
               (list (format-status) " " (format-track)
                     " ("
                     (pcase listen-lighter-format
                       ('remaining (concat "-" (format-time (- (listen--length listen-player)
                                                               (listen--elapsed listen-player)))))
                       (_ (concat (format-time (listen--elapsed listen-player))
                                  "/"
                                  (format-time (listen--length listen-player)))))
                     ") ")
             (list "")))))

(defun listen--mode-line-update (&rest _ignore)
  "Force updating of all mode lines when EMP is active."
  (when (and listen-player (listen--running-p listen-player))
    (setf listen-mode-lighter (listen-mode-lighter))))

;;;; Commands

(defun listen-pause (player)
  (interactive (list listen-player))
  (listen--pause player))

(defun listen-stop (player)
  (interactive (list listen-player))
  (listen--stop player))

(defun listen-play (player file)
  (interactive
   (list (listen--player)
         (read-file-name "Play file: " listen-directory nil t)))
  (listen--play player file))

(defun listen-volume (volume)
  "Set volume to VOLUME %."
  (interactive
   (let ((volume (floor (listen--volume (listen--player)))))
     (list (read-number "Volume %: " volume))))
  (listen--volume (listen--player) volume))

;;;; Transient

(require 'transient)

(transient-define-prefix listen-menu ()
  "Show Listen menu."
  :refresh-suffixes t
  [["Listen"
    :description
    (lambda ()
      (if listen-player
          (concat "Listening: " (listen-mode-lighter))
        "Not listening"))
    ("SPC" "Pause" listen-pause)
    ("p" "Play" listen-play)
    ("s" "Stop" listen-stop)
    ]]
  )

(provide 'listen)

;;; listen.el ends here
