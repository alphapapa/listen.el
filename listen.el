;;; listen.el --- Audio/Music player                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; Keywords: multimedia
;; Package-Requires: ((emacs "29.1") (persist "0.6") (taxy "0.10") (taxy-magit-section "0.13") (transient "0.5.3"))
;; Version: 0.9
;; URL: https://github.com/alphapapa/listen.el

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

;; This package aims to provide a simple audio/music player for Emacs.
;; It should "just work," with little-to-no configuration, have
;; intuitive commands, and be easily extended and customized.
;; (Contrast to setting up EMMS, or having to configure external
;; players like MPD.)  A `transient' menu, under the command `listen',
;; is the primary entry point.

;; The only external dependency is VLC, which is currently the only
;; player backend that is supported.  (Other backends may easily be
;; added; see library `listen-vlc' for example.)  Track metadata is
;; read using EMMS's native Elisp metadata library, which has been
;; imported into this package.

;; Queues are provided as the means to play consecutive tracks, and
;; they are shown in a `vtable'-based view buffer.  They are persisted
;; between sessions using the `persist' library, and they may be
;; bookmarked.

;; The primary interface to one's music library is through the
;; filesystem, by selecting a file to play, or by adding files and
;; directories to a queue.  Although MPD is not required, support is
;; provided for finding files from a local MPD server's library using
;; MPD's metadata searching.

;; Note a silly limitation: a track may be present in a queue only
;; once (but who would want to have a track more than once in a
;; playlist).

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'listen-lib)
(require 'listen-vlc)

;;;; Variables

(defvar listen-mode-update-mode-line-timer nil)
(defvar listen-queue-repeat-mode)

;;;; Customization

(defgroup listen nil
  "Audio/Music player."
  :group 'multimedia
  :link '(url-link "https://github.com/alphapapa/listen.el")
  :link '(emacs-commentary-link "listen")
  :link '(emacs-library-link "listen"))

(defcustom listen-directory "~/Music"
  "Default music directory."
  :type 'directory)

(defcustom listen-lighter-title-max-length 19
  "Truncate track titles to this many characters.
Used for mode line lighter and transient menu.  Since the
truncation precision specifier that may be used in
`listen-lighter-format' can't add an ellipsis where truncation
happens, this option truncates before that format spec is applied
and adds an ellipsis where it occurs."
  :type 'natnum)

(defcustom listen-lighter-format "🎵:%s %a: %t (%r)%E "
  "Format for mode line lighter.
Uses `format-spec', which see.  These format specs are available:

%a: Artist
%A: Album
%t: Title

%e: Elapsed time
%r: Remaining time
%s: Player status icon

%E: Extra data specified in `listen-lighter-extra-functions',
    which see."
  :type 'string)

(defcustom listen-lighter-extra-functions nil
  "Functions to show extra info in the lighter.
Each is called without arguments and should return a string
without extra whitespace."
  :type '(repeat (choice (const :tag "Remaining queue time" listen-queue-format-remaining)
                         (const :tag "Track rating" listen-lighter-format-rating)
                         function)))

(defcustom listen-track-end-functions '(listen-play-next)
  "Functions called when a track finishes playing.
Called with one argument, the player (if the player has a queue,
its current track will be the one that just finished playing)."
  :type 'hook)

;;;; Commands

(defun listen-quit (player)
  "Quit PLAYER.
Interactively, uses the default player."
  (interactive
   (list (listen-current-player)))
  (delete-process (listen-player-process player))
  (when (eq player listen-player)
    (setf listen-player nil))
  (listen-mode--update))

(declare-function listen-queue-next "listen-queue")
(defun listen-next (player)
  "Play next track in PLAYER's queue.
Interactively, uses the default player."
  (interactive (list (listen-current-player)))
  (listen-queue-next (map-elt (listen-player-etc player) :queue)))

(defun listen-pause (player)
  "Pause/unpause PLAYER.
Interactively, uses the default player."
  (interactive (list (listen-current-player)))
  (listen--pause player))

;; (defun listen-stop (player)
;;   (interactive (list listen-player))
;;   (listen--stop player))

;;;###autoload
(defun listen-play (player file)
  "Play FILE with PLAYER.
Interactively, uses the default player."
  (interactive
   (list (listen-current-player)
         (read-file-name "Play file: " listen-directory nil t)))
  (listen--play player file))

(defun listen-volume (player volume)
  "Set PLAYER's volume to VOLUME %.
Interactively, uses the default player."
  ;; TODO: Relative volume (at least for VLC).
  (interactive
   (let* ((player (listen-current-player))
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
   (let* ((player (listen-current-player))
          (position (read-string "Seek to position: "))
          (prefix (when (string-match (rx bos (group (any "-+")) (group (1+ anything))) position)
                    (prog1 (match-string 1 position)
                      (setf position (match-string 2 position)))))
          (seconds (listen-read-time position)))
     (list player (concat prefix (number-to-string seconds)))))
  (listen--seek player seconds))

(cl-defun listen-shell-command (command filenames)
  "Run shell COMMAND on FILENAMES.
Interactively, use the current player's current track, and read
command with completion."
  (interactive
   (let* ((player (listen-current-player))
          (filenames (list (abbreviate-file-name (listen--filename player))))
          (command (read-shell-command (format "Run command on %S: " filenames))))
     (list command filenames)))
  (let ((command (format "%s %s" command
                         (mapconcat (lambda (filename)
                                      (shell-quote-argument (expand-file-name filename)))
                                    filenames " ")))
        (display-buffer-alist `((,(regexp-quote shell-command-buffer-name-async) display-buffer-no-window))))
    (async-shell-command command)))

(defun listen-jump (track)
  "Jump to TRACK in a Dired buffer.
Interactively, jump to current queue's current track."
  (interactive (list (listen-queue-current (map-elt (listen-player-etc (listen-current-player)) :queue))))
  (dired-jump-other-window (listen-track-filename track)))

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
  "Return lighter for `listen-mode'.
According to `listen-lighter-format', which see."
  (when-let ((listen-player)
             ((listen--running-p listen-player))
             ((listen--playing-p listen-player))
             (info (listen--info listen-player)))
    (format-spec listen-lighter-format
                 `((?a . ,(lambda ()
                            (or (alist-get "artist" info nil nil #'equal) "")))
                   (?A . ,(lambda ()
                            (or (alist-get "album" info nil nil #'equal) "")))
                   (?t . ,(lambda ()
                            (if-let ((title (alist-get "title" info nil nil #'equal)))
                                (truncate-string-to-width title listen-lighter-title-max-length
                                                          nil nil t)
                              "")))
                   (?e . ,(lambda ()
                            (listen-format-seconds (listen--elapsed listen-player))))
                   (?r . ,(lambda ()
                            (concat "-" (listen-format-seconds
                                         (- (listen--length listen-player)
                                            (listen--elapsed listen-player))))))
                   (?s . ,(lambda ()
                            (pcase (listen--status listen-player)
                              ("playing" "▶")
                              ("paused" "⏸")
                              ("stopped" "■")
                              (_ ""))))
                   (?E . ,(lambda ()
                            (if-let ((extra (mapconcat #'funcall listen-lighter-extra-functions " ")))
                                (concat " " extra)
                              "")))))))

(defun listen-lighter-format-rating ()
  "Return the rating of the current track for display in the lighter."
  (when-let ((player (listen-current-player))
             (queue (map-elt (listen-player-etc player) :queue))
             (track (listen-queue-current queue))
             (rating (or (listen-track-rating track)
                         (map-elt (listen-track-etc track) "fmps_rating"))))
    (unless (equal "-1" rating)
      (format "[%s]" (* 5 (string-to-number rating))))))

(declare-function listen-queue-play "listen-queue")
(declare-function listen-queue-next-track "listen-queue")
(defun listen-mode--update (&rest _ignore)
  "Play next track and/or update variable `listen-mode-lighter'."
  (let (playing-next-p)
    (when listen-player
      (unless (or (listen--playing-p listen-player)
                  ;; HACK: It seems that sometimes the player gets restarted
                  ;; even when paused: this extra check should prevent that.
                  (member (listen--status listen-player) '("playing" "paused")))
        (setf playing-next-p
              (run-hook-with-args 'listen-track-end-functions listen-player))))
    (setf listen-mode-lighter
          (when (and listen-player (listen--running-p listen-player))
            (listen-mode-lighter)))
    (when playing-next-p
      ;; TODO: Remove this (I think it's not necessary anymore).
      (force-mode-line-update 'all))))

(defun listen-play-next (player)
  "Play PLAYER's queue's next track and return non-nil if playing."
  (when-let ((queue (map-elt (listen-player-etc player) :queue)))
    (if-let ((next-track (listen-queue-next-track queue)))
        (progn
          (listen-queue-play queue next-track)
          t)
      ;; Queue done: repeat?
      (pcase listen-queue-repeat-mode
        ('queue
         (listen-queue-play queue)
         t)
        ('shuffle
         (listen-queue-play queue (seq-random-elt (listen-queue-tracks queue)))
         (listen-queue-shuffle queue)
         t)))))

;;;; Functions

(defun listen-read-time (time)
  "Return TIME in seconds.
TIME is a string like \"SS\", \"MM:SS\", or \"HH:MM:SS\"."
  (unless (string-match (rx (group (1+ num))
                            (optional ":" (group (1+ num))
                                      (optional ":" (group (1+ num)))))
                        time)
    (user-error "TIME must be a string like \"SS\", \"MM:SS\", or \"HH:MM:SS\""))
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

(declare-function listen-queue "listen-queue")
(declare-function listen-queue-shuffle "listen-queue")

(defvar listen-queue-repeat-mode)

;; It seems that autoloading the transient prefix command doesn't work
;; as expected, so we'll try this workaround.

;;;###autoload (autoload 'listen-menu "listen" nil t)
(transient-define-prefix listen-menu ()
  "Show Listen menu."
  :info-manual "(listen)"
  :refresh-suffixes t
  ["Listen"
   :description
   ;; TODO: Try using `transient-info' class for this line.
   (lambda ()
     (if listen-player
         (concat "Listening: " (listen-mode-lighter))
       "Not listening"))
   ;; Getting this layout to work required a lot of trial-and-error.
   [("Q" "Quit" listen-quit
     :inapt-if-not (lambda ()
                     listen-player))]
   [("m" "Metadata" listen-view-track
     :inapt-if-not (lambda ()
                     (and listen-player
                          (listen--playing-p listen-player))))]]
  [["Player"
    :if (lambda ()
          listen-player)
    ("SPC" "Pause" listen-pause)
    ("p" "Play" listen-play)
    ;; ("ESC" "Stop" listen-stop)
    ("n" "Next" listen-next)
    ("s" "Seek" listen-seek)]
   ["Volume"
    :if (lambda ()
          listen-player)
    :description
    (lambda ()
      (if listen-player
          (format "Volume: %.0f%%" (listen--volume listen-player))
        "Volume: N/A"))
    ("=" "Set" listen-volume)
    ("v" "Down" (lambda ()
                  (interactive)
                  (let ((player (listen-current-player)))
                    (listen-volume player (max 0 (- (listen--volume player) 5)))))
     :transient t)
    ("V" "Up" (lambda ()
                (interactive)
                (let* ((player (listen-current-player))
                       (max-volume (listen-player-max-volume player)))
                  (listen-volume player (min max-volume (+ (listen--volume player) 5)))))
     :transient t)]
   ["Repeat"
    ("rn" "None" (lambda () (interactive) (setopt listen-queue-repeat-mode nil))
     :inapt-if (lambda () (not listen-queue-repeat-mode)))
    ("rq" "Queue" (lambda () (interactive) (setopt listen-queue-repeat-mode 'queue))
     :inapt-if (lambda () (eq 'queue listen-queue-repeat-mode)))
    ("rs" "Queue and shuffle" (lambda () (interactive) (setopt listen-queue-repeat-mode 'shuffle))
     :inapt-if (lambda () (eq 'shuffle listen-queue-repeat-mode)))
    ;; ("qrt" "Track" (lambda () (interactive) (setopt listen-queue-repeat-mode 'track))
    ;;  :inapt-if (lambda () (eq 'track listen-queue-repeat-mode)))
    ]

   ["Library view"
    ("lf" "from files" listen-library)
    ("lm" "from MPD" listen-library-from-mpd)
    ("lq" "from queue" listen-library-from-queue)
    ("lp" "from playlist file" listen-library-from-playlist-file)]]

  [["Queue mode"
    :description
    (lambda ()
      (if-let ((player listen-player)
               (queue (map-elt (listen-player-etc player) :queue)))
          (format "Queue: %s (track %s/%s)" (listen-queue-name queue)
                  (cl-position (listen-queue-current queue) (listen-queue-tracks queue))
                  (length (listen-queue-tracks queue)))
        "No queue"))
    ("ql" "List" listen-queue-list)
    ("qq" "View current" (lambda ()
                           "View current queue."
                           (interactive)
                           (listen-queue (map-elt (listen-player-etc (listen-current-player)) :queue)))
     :if (lambda ()
           (if-let ((player listen-player))
               (map-elt (listen-player-etc player) :queue))))
    ("qo" "View other" listen-queue)
    ("qp" "Play other" listen-queue-play
     :transient t)
    ("qn" "New" listen-queue-new
     :transient t)
    ("qD" "Discard" listen-queue-discard
     :transient t)]
   ["Tracks"
    ("qj" "Jump to current in Dired" listen-jump)
    ("qt" "Play track" (lambda ()
                         "Call `listen-queue-play' with prefix."
                         (interactive)
                         (let ((current-prefix-arg '(4)))
                           (call-interactively #'listen-queue-play)))
     :transient t)
    ("qd" "Deduplicate" listen-queue-deduplicate
     :transient t)
    ("qs" "Shuffle" (lambda () (interactive) (call-interactively #'listen-queue-shuffle))
     :transient t)]
   ["Add tracks"
    ("qaf" "from files" listen-queue-add-files
     :transient t)
    ("qam" "from MPD" listen-queue-add-from-mpd
     :transient t)
    ("qap" "from playlist file" listen-queue-add-from-playlist-file
     :transient t)]])

;; NOTE: This alias must come after the command it refers to, otherwise the autoload file fails to
;; finish loading (without warning), which breaks a lot of things!
;;;###autoload
(defalias 'listen #'listen-menu)

(provide 'listen)

;;; listen.el ends here
