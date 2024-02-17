;;; listen-queue.el --- Listen queue                 -*- lexical-binding: t; -*-

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

(require 'vtable)

(require 'listen-lib)

(defvar listen-queues nil)

(defgroup listen-queue nil
  "Queues."
  :group 'listen)

(defcustom listen-queue-time-format "%Y-%m-%d %H:%M:%S"
  "Time format for `listen-queue' buffer."
  :type 'string)

(defmacro listen-queue-command (command)
  "Expand to a lambda that applies its args to COMMAND and reverts the list buffer."
  `(lambda (&rest args)
     (let ((list-buffer (current-buffer)))
       (apply #',command queue args)
       (with-current-buffer list-buffer
         (vtable-revert)))))

;;;###autoload
(defun listen-queue (queue)
  "Show listen QUEUE."
  (interactive (list (listen-queue-complete)))
  (with-current-buffer (get-buffer-create (format "*Listen Queue: %s*" (listen-queue-name queue)))
    (let ((inhibit-read-only t))
      (read-only-mode)
      (erase-buffer)
      (toggle-truncate-lines 1)
      (make-vtable
       :columns
       (list (list :name "*" :primary 'descend
                   :getter (lambda (track _table)
                             (if (eq track (listen-queue-current queue))
                                 "▶" " ")))
             (list :name "At" :primary 'descend
                   :getter (lambda (track _table)
                             (cl-position track (listen-queue-tracks queue))))
             (list :name "Artist"
                   :getter (lambda (track _table)
                             (listen-track-artist track)))
             (list :name "Title"
                   :getter (lambda (track _table)
                             (listen-track-title track)))
             (list :name "Album"
                   :getter (lambda (track _table)
                             (listen-track-album track)))
             (list :name "#"
                   :getter (lambda (track _table)
                             (listen-track-number track)))
             (list :name "Date"
                   :getter (lambda (track _table)
                             (listen-track-date track)))
             (list :name "File"
                   :getter (lambda (track _table)
                             (listen-track-filename track))))
       :objects-function (lambda ()
                           (listen-queue-tracks queue))
       :sort-by '((1 . descend))
       :actions `("q" (lambda (&rest _) (bury-buffer))
                  "n" (lambda (&rest _) (forward-line 1))
                  "p" (lambda (&rest _) (forward-line -1))
                  "RET" ,(listen-queue-command listen-queue-play)))
      (pop-to-buffer (current-buffer)))))

(defun listen-queue-play (queue track)
  (listen-play (listen--player) (listen-track-filename track))
  (setf (listen-queue-current queue) track))

(defun listen-queue--format-time (time)
  "Return TIME formatted according to `listen-queue-time-format', which see."
  (if time
      (format-time-string listen-queue-time-format time)
    "never"))

(defun listen-queue-complete ()
  "Return a Listen queue selected with completion."
  (let* ((queue-names (mapcar #'listen-queue-name listen-queues))
         (selected (completing-read "Queue (or enter new name): " queue-names)))
    (if (member selected queue-names)
        (cl-find selected listen-queues :key #'listen-queue-name :test #'equal)
      (push (make-listen-queue :name selected) listen-queues))))

(cl-defun listen-queue-add (queue files)
  (interactive
   (let ((queue (listen-queue-complete))
         (path (expand-file-name (read-file-name "Enqueue file/directory: " listen-directory nil t))))
     (list queue
           (if (file-directory-p path)
               (directory-files-recursively path ".")
             (list path)))))
  (cl-callf append (listen-queue-tracks queue) (mapcar #'listen-queue-track files)))

(require 'emms-info-native)

(defun listen-queue-track (filename)
  "Return track for FILENAME."
  (let* ((metadata (emms-info-native--decode-info-fields filename)))
    (make-listen-track :filename filename
                       :artist (map-elt metadata "artist")
                       :title (map-elt metadata "title")
                       :album (map-elt metadata "album")
                       :number (map-elt metadata "tracknumber")
                       :date (map-elt metadata "date")
                       :genre (map-elt metadata "genre"))))

(defun listen-queue-next-p (queue)
  "Return non-nil if QUEUE has a track after current."
  (cl-subseq (listen-queue-tracks queue) (listen-queue-track-number queue)))

(provide 'listen-queue)
;;; listen-queue.el ends here
