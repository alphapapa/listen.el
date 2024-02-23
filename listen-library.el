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
(cl-defun listen-library (paths &key name)
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
         (buffer (thread-last listen-library-taxy
                              taxy-emptied
                              (taxy-fill tracks)
                              ;; (taxy-sort #'string< #'listen-queue-track-)
                              (taxy-sort* #'string< #'taxy-name)
                              taxy-magit-section-pp))
         (buffer-name (if name
                          (format "*Listen library: %s" name)
                        (generate-new-buffer-name (format "*Listen library*")))))
    (pop-to-buffer buffer)
    (rename-buffer buffer-name)
    (setq-local bookmark-make-record-function #'listen-library--bookmark-make-record)
    (setf listen-library-paths paths
          listen-library-name name)))

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
