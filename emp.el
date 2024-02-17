;;; emp.el --- Emacs Music Player                    -*- lexical-binding: t; -*-

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

;;;; Types

(cl-defstruct emp-player
  process command args)

;;;; Variables

(defvar emp-player nil)

(defvar emp-mode-update-mode-line-timer nil)

;;;; Functions

(cl-defmethod emp-play ((player emp-vlc) &key file))

(cl-defmethod emp-running-p ((player emp-player))
  "Return non-nil if PLAYER is running."
  (process-live-p (emp-player-process player)))

(defvar emp-mode-lighter nil)

(define-minor-mode emp-mode
  "Show EMP player status in the mode line."
  :global t
  (let ((lighter '(emp-mode emp-mode-lighter)))
    (if emp-mode
        (progn
          (when (timerp emp-mode-update-mode-line-timer)
            ;; Cancel any existing timer.  Generally shouldn't happen, but not impossible.
            (cancel-timer emp-mode-update-mode-line-timer))
          (setf emp-mode-update-mode-line-timer (run-with-timer 1 1 #'emp--mode-line-update))
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew lighter global-mode-string :test #'equal))
      (when emp-mode-update-mode-line-timer
        (cancel-timer emp-mode-update-mode-line-timer)
        (setf emp-mode-update-mode-line-timer nil))
      (setf global-mode-string
            (remove lighter global-mode-string)))))

(defcustom emp-lighter-format 'remaining
  "Time elapsed/remaining format."
  :type '(choice (const remaining)
                 (const elapsed)))

(defun emp-mode-lighter ()
  "Return lighter for `emp-mode'."
  (cl-labels ((format-time (seconds)
                (format-seconds "%h:%.2m:%.2s%z" seconds))
              (format-track ()
                (emp-title emp-player))
              (format-status ()
                (pcase (emp--status emp-player)
                  ("playing" "▶")
                  ("paused" "⏸")
                  ("stopped" "■"))))
    (if (emp-playing-p emp-player)
        (concat "🎵 "
                (format-track)
                " ("
                (pcase emp-lighter-format
                  ('remaining (concat "-" (format-time (- (emp-length emp-player)
                                                          (emp-elapsed emp-player)))))
                  (_ (concat (format-time (emp-elapsed emp-player))
                             "/"
                             (format-time (emp-length emp-player)))))
                ")" (format-status) " ")
      "")))

(defun emp--mode-line-update (&rest _ignore)
  "Force updating of all mode lines when EMP is active."
  (when (and emp-player (emp-running-p emp-player))
    (setf emp-mode-lighter (emp-mode-lighter ))
    ;; (force-mode-line-update 'all)
    ))

(defun emp-pause (player)
  (interactive (list emp-player))
  (emp--pause player))

(defun emp-stop (player)
  (interactive (list emp-player))
  (emp--stop player))

(provide 'emp)

;;; emp.el ends here
