;;; listen-library.el --- Music library              -*- lexical-binding: t; -*-

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

;; This implements a basic "library" view based on `taxy' and
;; `taxy-magit-section'.

;; TODO: Commands to add tracks to queues from this view.
;; TODO: Commands to view a queue's files in library view.

;;; Code:

;;;; Requirements

(require 'taxy)
(require 'taxy-magit-section)

(require 'listen-lib)
(require 'listen-queue)

;;;; Variables

(defvar listen-directory)

(defvar-local listen-library-name nil)
(defvar-local listen-library-paths nil)

(defvar listen-library-taxy
  (cl-labels ((with-face (face string)
                (when (and string (not (string-empty-p string)))
                  (propertize string 'face face)))
              (genre (track)
                (or (listen-track-genre track) "[unknown genre]"))
              (date (track)
                (or (listen-track-date track) "[unknown date]"))
              (artist (track)
                (or (with-face 'listen-artist (listen-track-artist track))
                    "[unknown artist]"))
              (album (track)
                (or (when-let ((album (with-face 'listen-album (listen-track-album track))))
                      (concat album
                              (pcase (listen-track-date track)
                                (`nil nil)
                                (date (format " (%s)" date)))))
                    "[unknown album]"))
              (number (track)
                (or (listen-track-number track) ""))
              (title (track)
                (concat (pcase (number track)
                          ("" "")
                          (else (format "%s: " else)))
                        (or (with-face 'listen-title (listen-track-title track))
                            "[unknown title]")))
              (format-track (track)
                (let* ((duration (listen-track-duration track)))
                  (when duration
                    (setf duration (concat "(" (listen-format-seconds duration) ")" " ")))
                  (concat duration (listen-track-filename track))))
              (make-fn (&rest args)
                (apply #'make-taxy-magit-section
                       :make #'make-fn
                       :format-fn #'format-track
                       args)))
    (make-fn
     :name "Genres"
     :take (apply-partially #'taxy-take-keyed
                            (list #'genre #'artist ;; #'date
                                  #'album #'title)))))

;;;; Mode

(declare-function listen-menu "listen")
(declare-function listen-jump "listen")

(defvar-keymap listen-library-mode-map
  :parent magit-section-mode-map
  "?" #'listen-menu
  "!" #'listen-library-shell-command
  "a" #'listen-library-add-tracks
  "g" #'listen-library-revert
  "j" #'listen-library-jump
  "RET" #'listen-library-play-or-add)

(define-derived-mode listen-library-mode magit-section-mode "Listen-Library"
  (setq-local bookmark-make-record-function #'listen-library--bookmark-make-record))

;;;###autoload
(cl-defun listen-library (paths &key name buffer)
  "Show a library view of PATHS.
PATHS is a list of paths to files and/or directories.
Interactively, with prefix, NAME may be specified to show in the
mode line and bookmark name.  BUFFER may be specified in which to
show the view."
  (interactive
   (list (list (read-file-name "View library for: "))
         :name (when current-prefix-arg
                 (read-string "Library name: "))))
  (let* ((filenames (cl-loop for path in paths
                             if (file-directory-p path)
                             append (directory-files-recursively path "." t)
                             else collect path))
         (tracks (listen-queue-tracks-for filenames))
         (buffer-name (if name
                          (format "*Listen library: %s" name)
                        (generate-new-buffer-name (format "*Listen library*"))))
         (buffer (or buffer (get-buffer-create buffer-name)))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (listen-library-mode)
      (setf listen-library-paths paths
            listen-library-name name)
      (erase-buffer)
      (thread-last listen-library-taxy
                   taxy-emptied
                   (taxy-fill tracks)
                   ;; (taxy-sort #'string< #'listen-queue-track-)
                   (taxy-sort* #'string< #'taxy-name)
                   taxy-magit-section-insert))
    (pop-to-buffer buffer)))

;;;; Commands

(defun listen-library-add-tracks (queue tracks)
  "Add TRACKS to QUEUE.
Interactively, play tracks in sections at point and select QUEUE
with completion."
  (interactive
   (list (listen-queue-complete :allow-new-p t)
         (listen-library--selected-tracks)))
  (listen-queue-add-files (mapcar #'listen-track-filename tracks) queue))

(declare-function listen-play "listen")
(defun listen-library-play-or-add (tracks &optional queue)
  "Play or add TRACKS.
If TRACKS is a list of one track, play it immediately; otherwise
prompt for a QUEUE to add them to."
  (interactive
   (let ((tracks (listen-library--selected-tracks)))
     (list tracks (when (length> tracks 1)
                    (listen-queue-complete :prompt "Add tracks to queue" :allow-new-p t)))))
  (if queue
      (listen-queue-add-files (mapcar #'listen-track-filename tracks) queue)
    (listen-play (listen--player) (listen-track-filename (car tracks)))))

(defun listen-library-jump (track)
  "Jump to TRACK in a Dired buffer."
  (interactive
   (list (car (listen-library--selected-tracks))))
  (listen-jump track))

(declare-function listen-shell-command "listen")
(defun listen-library-shell-command (command filenames)
  "Run COMMAND on FILENAMES.
Interactively, read COMMAND and use tracks at point in
`listen-library' buffer."
  (interactive
   (let* ((filenames (mapcar #'listen-track-filename (listen-library--selected-tracks)))
          (command (read-shell-command (format "Run command on %S: " filenames))))
     (list command filenames)))
  (listen-shell-command command filenames))

(defun listen-library-revert ()
  "Revert current listen library buffer."
  (interactive)
  (cl-assert listen-library-paths)
  (listen-library listen-library-paths :name listen-library-name :buffer (current-buffer)))

(declare-function listen-mpd-completing-read "listen-mpd")
;;;###autoload
(cl-defun listen-library-from-mpd (filenames)
  "Show library view of FILENAMES selected from MPD library."
  (interactive
   (list (listen-mpd-completing-read :select-tag-p t)))
  (listen-library filenames))

(cl-defun listen-library-from-playlist-file (filename)
  "Show library view tracks in playlist at FILENAME."
  (interactive
   (list (read-file-name "Add tracks from playlist: " listen-directory nil t nil
                         (lambda (filename)
                           (pcase (file-name-extension filename)
                             ("m3u" t))))))
  (listen-library (listen-queue--m3u-filenames filename)))

;;;; Functions

(defun listen-library--selected-tracks ()
  "Return tracks in highlighted sections or ones at point."
  (cl-labels ((value-of (section)
                (let ((value (oref section value)))
                  (cl-typecase value
                    (listen-track (list value))
                    (taxy-magit-section (taxy-flatten value))))))
    ;; NOTE: `magit-region-sections' only returns non-nil if the
    ;; region starts and ends at sibling sections.  Even if, e.g. the
    ;; region end is on a section that's a descendant of a sibling of
    ;; the section at the region start (a situation which seems like
    ;; it ought to return sections in the region), it returns nil.
    ;; This may be confusing to users, but it seems like an upstream
    ;; issue.
    (or (flatten-list (mapcar #'value-of (magit-region-sections)))
        (value-of (magit-current-section)))))

;;;;; Bookmark support

(require 'bookmark)

(defun listen-library--bookmark-make-record ()
  "Return a bookmark record for the current library buffer."
  (cl-assert listen-library-paths)
  `(,(format "Listen library: %s" (or listen-library-name listen-library-paths))
    (handler . listen-library--bookmark-handler)
    (name . ,listen-library-name)
    (paths . ,listen-library-paths)))

;;;###autoload
(defun listen-library--bookmark-handler (bookmark)
  "Set current buffer to BOOKMARK's listen library."
  (let* ((paths (bookmark-prop-get bookmark 'paths))
         (name (bookmark-prop-get bookmark 'name)))
    (listen-library paths :name name)))

(provide 'listen-library)

;;; listen-library.el ends here
