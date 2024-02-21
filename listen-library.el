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
(defun listen-library (paths)
  "Show a library view of PATHS.
PATHS is a list of paths to files and/or directories."
  (interactive (list (list (read-file-name "View library for: "))))
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
                              taxy-magit-section-pp)))
    (pop-to-buffer buffer)
    (rename-buffer (generate-new-buffer-name (format "*Listen Library*")))))

(provide 'listen-library)

;;; listen-library.el ends here
