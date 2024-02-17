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

;;;###autoload
(defun listen-queue ()
  "Show listen queue."
  (interactive)
  (with-current-buffer (get-buffer-create "*Listen: Queue*")
    (let ((inhibit-read-only t))
      (read-only-mode)
      (erase-buffer)
      )))

(cl-defun listen-queue-add (&optional (queue listen-queue) &rest files)
  (interactive
   (list listen-queue
         (read-file-name "Enqueue file: " listen-directory nil t)))
  (cl-callf append (listen-queue-tracks queue) (mapcar #'listen-queue-track files)))

(defun listen-queue-track (file)
  "Return track for FILE."
  (make-listen-track :filename file))

(defun listen-queue-next-p (queue)
  "Return non-nil if QUEUE has a track after current."
  (cl-subseq (listen-queue-tracks queue) (listen-queue-track-number queue)))

(provide 'listen-queue)
;;; listen-queue.el ends here
