;;; listen-library.el --- Music library              -*- lexical-binding: t; -*-

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

;; This implements a basic "library" view based on `taxy' and
;; `taxy-magit-section'.

;; TODO: Commands to add tracks to queues from this view.
;; TODO: Commands to view a queue's files in library view.

;;; Code:

(require 'taxy)
(require 'taxy-magit-section)

(require 'listen-lib)
(require 'listen-queue)

(defvar-local listen-library-name nil)
(defvar-local listen-library-paths nil)

(defvar listen-library-taxy
  (cl-labels ((genre (track)
                (or (listen-track-genre track) "[unknown genre]"))
              (date (track)
                (or (listen-track-date track) "[unknown date]"))
              (artist (track)
                (or (listen-track-artist track) "[unknown artist]"))
              (album (track)
                (or (listen-track-album track) "[unknown album]"))
              (title (track)
                (or (listen-track-title track) "[unknown title]"))
              (number (track)
                (or (listen-track-number track) ""))
              (track-string (track)
                (concat (pcase (number track)
                          ("" "")
                          (else (format "%s: " else)))
                        (title track)))
              (make-fn (&rest args)
                (apply #'make-taxy-magit-section
                       :make #'make-fn
                       :format-fn #'cl-prin1-to-string
                       args)))
    (make-fn
     :name "Genres"
     :take (apply-partially #'taxy-take-keyed
                            (list #'genre #'artist #'date #'album #'track-string)))))

;;;###autoload
(cl-defun listen-library (paths &key name buffer)
  "Show a library view of PATHS.
PATHS is a list of paths to files and/or directories.
Interactively, with prefix, NAME may be specified to show in the
mode line and bookmark name."
  (interactive
   (list (list (read-file-name "View library for: "))
         :name (when current-prefix-arg
                 (read-string "Library name: "))))
  (let* ((filenames (cl-loop for path in paths
                             if (file-directory-p path)
                             append (directory-files-recursively path "." t)
                             else collect path))
         (tracks (remq nil (mapcar #'listen-queue-track filenames)))
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

(defvar-keymap listen-library-mode-map
  :parent magit-section-mode-map
  "!" #'listen-library-shell-command
  "a" #'listen-library-add-tracks
  "g" #'listen-library-revert
  "RET" #'listen-library-play-or-add)

(defun listen-library-add-tracks (queue tracks)
  "Add TRACKS to QUEUE.
Interactively, play tracks in sections at point and select QUEUE
with completion."
  (interactive
   (list (listen-queue-complete) (listen-library--tracks-at-point)))
  (listen-queue-add-files (mapcar #'listen-track-filename tracks) queue))

(declare-function listen-play "listen")
(defun listen-library-play-or-add (tracks &optional queue)
  "Play or add TRACKS.
If TRACKS is a list of one track, play it immediately; otherwise
prompt for a QUEUE to add them to."
  (interactive
   (let ((tracks (listen-library--tracks-at-point)))
     (list tracks
           (when (length> tracks 1)
             (listen-queue-complete)))))
  (if queue
      (listen-queue-add-files (mapcar #'listen-track-filename tracks) queue)
    (listen-play (listen--player) (listen-track-filename (car tracks)))))

(defun listen-library--tracks-at-point ()
  "Return tracks in sections at point."
  (let ((value(oref (magit-current-section) value)))
    (cl-typecase value
      (listen-track (list value))
      (taxy-magit-section (taxy-flatten value)))))

(define-derived-mode listen-library-mode magit-section-mode "Listen-Library"
  (setq-local bookmark-make-record-function #'listen-library--bookmark-make-record))

(declare-function listen-shell-command "listen")
(defun listen-library-shell-command (command filenames)
  "Run COMMAND on FILENAMES.
Interactively, read COMMAND and use tracks at point in
`listen-library' buffer."
  (interactive
   (let* ((filenames (mapcar #'listen-track-filename (listen-library--tracks-at-point)))
          (command (read-shell-command (format "Run command on %S: " filenames))))
     (list command filenames)))
  (listen-shell-command command filenames))

(defun listen-library-revert ()
  "Revert current listen library buffer."
  (interactive)
  (cl-assert listen-library-paths)
  (listen-library listen-library-paths :name listen-library-name :buffer (current-buffer)))

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
