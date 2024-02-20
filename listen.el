;;; listen.el --- Music player                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:
;; Package-Requires: ((emacs "29.1") (emms "11") (persist "0.6"))

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
(require 'map)

(require 'listen-lib)
(require 'listen-vlc)

;;;; Variables

(defvar listen-mode-update-mode-line-timer nil)

;;;; Customization

(defgroup listen nil
  "Music player."
  :group 'applications)

(defcustom listen-directory "~/Music"
  "Default music directory."
  :type 'directory)

;;;; Commands

(defun listen-quit (player)
  "Quit PLAYER.
Interactively, uses the default player."
  (interactive
   (list (listen--player)))
  (delete-process (listen-player-process player))
  (when (eq player listen-player)
    (setf listen-player nil)))

(declare-function listen-queue-next "listen-queue")
(defun listen-next (player)
  "Play next track in PLAYER's queue.
Interactively, uses the default player."
  (interactive (list listen-player))
  (listen-queue-next (map-elt (listen-player-etc player) :queue)))

(defun listen-pause (player)
  "Pause/unpause PLAYER.
Interactively, uses the default player."
  (interactive (list listen-player))
  (listen--pause player))

;; (defun listen-stop (player)
;;   (interactive (list listen-player))
;;   (listen--stop player))

;;;###autoload
(defun listen-play (player file)
  "Play FILE with PLAYER.
Interactively, uses the default player."
  (interactive
   (list (listen--player)
         (read-file-name "Play file: " listen-directory nil t)))
  (listen--play player file))

(defun listen-volume (player volume)
  "Set PLAYER's volume to VOLUME %.
Interactively, uses the default player."
  ;; TODO: Relative volume (at least for VLC).
  (interactive
   (let* ((player (listen--player))
          (volume (floor (listen--volume player))))
     (list player (read-number "Volume %: " volume))))
  (listen--volume player volume)
  (message "Volume: %.0f%%" volume))

(defun listen-seek (player seconds)
  "Seek PLAYER to SECONDS.
Interactively, use the default player, and read a position
timestamp, like \"23\" or \"1:23\", with optional -/+ prefix for
relative seek."
  (interactive
   (let* ((player (listen--player))
          (position (read-string "Seek to position: "))
          (prefix (when (string-match (rx bos (group (any "-+")) (group (1+ anything))) position)
                    (prog1 (match-string 1 position)
                      (setf position (match-string 2 position)))))
          (seconds (listen-read-time position)))
     (list player (concat prefix (number-to-string seconds)))))
  (listen--seek player seconds))

(cl-defun listen-shell-command (command filename)
  "Run shell COMMAND on FILENAME.
Interactively, use the current player's current track, and read
command with completion."
  (interactive
   (let* ((player (listen--player))
          (filename (abbreviate-file-name (listen--filename player)))
          (command (read-shell-command (format "Run command on %S: " filename))))
     (list command filename)))
  (let ((command (format "%s %s" command (shell-quote-argument (expand-file-name filename)))))
    (async-shell-command command)))

;;;; Mode

(defvar listen-mode-lighter nil)

;;;###autoload
(define-minor-mode listen-mode
  "Listen to queues of tracks and show status in mode line."
  :global t
  (let ((lighter '(listen-mode listen-mode-lighter)))
    (if listen-mode
        (progn
          (when (timerp listen-mode-update-mode-line-timer)
            ;; Cancel any existing timer.  Generally shouldn't happen, but not impossible.
            (cancel-timer listen-mode-update-mode-line-timer))
          (setf listen-mode-update-mode-line-timer (run-with-timer nil 1 #'listen-mode--update))
          ;; Avoid adding the lighter multiple times if the mode is activated again.
          (cl-pushnew lighter global-mode-string :test #'equal))
      (when listen-mode-update-mode-line-timer
        (cancel-timer listen-mode-update-mode-line-timer)
        (setf listen-mode-update-mode-line-timer nil))
      (setf global-mode-string
            (remove lighter global-mode-string)))))

(defun listen-mode-lighter ()
  "Return lighter for `listen-mode'."
  (cl-labels ((format-time (seconds)
                (format-seconds "%h:%z%.2m:%.2s" seconds))
              (format-track ()
                (let ((info (listen--info listen-player)))
                  (format "%s: %s" (alist-get "artist" info nil nil #'equal)
                          (truncate-string-to-width (alist-get "title" info nil nil #'equal)
                                                    listen-mode-title-max-length nil nil t))))
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
             '("■ ")))))

(defcustom listen-mode-title-max-length 15
  "Truncate track titles to this many characters."
  :type 'natnum)

(defcustom listen-lighter-format 'remaining
  "Time elapsed/remaining format.
For the currently playing track."
  :type '(choice (const :tag "Time remaining" remaining)
                 (const :tag "Time elapsed/total" elapsed)))

(declare-function listen-queue-play "listen-queue")
(declare-function listen-queue-next-track "listen-queue")
(defun listen-mode--update (&rest _ignore)
  "Play next track and/or update variable `listen-mode-lighter'."
  (let (playing-next-p)
    (unless (or (listen--playing-p listen-player)
                ;; HACK: It seems that sometimes the player gets restarted
                ;; even when paused: this extra check should prevent that.
                (member (listen--status listen-player) '("playing" "paused")))
      (when-let ((queue (map-elt (listen-player-etc listen-player) :queue))
                 (next-track (listen-queue-next-track queue)))
        (listen-queue-play queue next-track)
        (setf playing-next-p t)))
    (setf listen-mode-lighter
          (when (and listen-player (listen--running-p listen-player))
            (listen-mode-lighter)))
    (when playing-next-p
      (force-mode-line-update 'all))))

;;;; Functions

(defun listen-read-time (time)
  "Return TIME in seconds.
TIME is an HH:MM:SS string."
  (string-match (rx (group (1+ num))
                    (optional ":" (group (1+ num))
                              (optional ":" (group (1+ num)))))
                time)
  (let ((fields (nreverse
                 (remq nil
                       (list (match-string 1 time)
                             (match-string 2 time)
                             (match-string 3 time)))))
        (factors [1 60 3600]))
    (cl-loop for field in fields
             for factor across factors
             sum (* (string-to-number field) factor))))

;;;; Transient

(require 'transient)

;;;###autoload
(transient-define-prefix listen ()
  "Show Listen menu."
  :refresh-suffixes t
  [["Listen"
    :description
    (lambda ()
      (if listen-player
          (concat "Listening: " (listen-mode-lighter))
        "Not listening"))
    ("q" "Quit" listen-quit)]]

  [["Player"
    ("SPC" "Pause" listen-pause)
    ("p" "Play" listen-play)
    ;; ("ESC" "Stop" listen-stop)
    ("n" "Next" listen-next)
    ("s" "Seek" listen-seek)]
   ["Volume"
    :description
    (lambda ()
      (if listen-player
          (format "Volume: %.0f%%" (listen--volume listen-player))
        "Volume: N/A"))
    ("=" "Set" listen-volume)
    ("v" "Down" (lambda ()
                  (interactive)
                  (let ((player (listen--player)))
                    (listen-volume player (max 0 (- (listen--volume player) 5)))))
     :transient t)
    ("V" "Up" (lambda ()
                (interactive)
                (let ((player (listen--player)))
                  (listen-volume player (min 100 (+ (listen--volume player) 5)))))
     :transient t)]]

  [["Queue mode"
    :description
    (lambda ()
      (if-let ((player listen-player)
               (queue (map-elt (listen-player-etc player) :queue)))
          (format "Queue: %s (track %s/%s)" (listen-queue-name queue)
                  (cl-position (listen-queue-current queue) (listen-queue-tracks queue))
                  (length (listen-queue-tracks queue)))
        "No queue"))
    ("Q" "Show" listen-queue
     :transient t)
    ("P" "Play another queue" listen-queue-play
     :transient t)
    ("N" "New" listen-queue-new
     :transient t)
    ("D" "Discard" listen-queue-discard
     :transient t)]
   ["Tracks"
    ("A" "Add files" listen-queue-add-files
     :transient t)
    ("M" "Add files from MPD" listen-queue-add-from-mpd
     :transient t)
    ("T" "Select track" (lambda ()
                          "Call `listen-queue-play' with prefix."
                          (interactive)
                          (let ((current-prefix-arg '(4)))
                            (call-interactively #'listen-queue-play)))
     :transient t)
    ("S" "Shuffle" (lambda ()
                     "Shuffle queue."
                     (interactive)
                     (call-interactively #'listen-queue-shuffle))
     :transient t)]])

(provide 'listen)

;;; listen.el ends here
