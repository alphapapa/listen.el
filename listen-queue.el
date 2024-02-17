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

(require 'emms-info-native)
(require 'vtable)

(require 'listen-lib)

(defvar listen-queues nil)
(defvar listen-directory)

(defvar-local listen-queue nil
  "Queue in this buffer.")

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
      (setf listen-queue queue)
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
             (list :name "Artist" :max-width 20
                   :getter (lambda (track _table)
                             (listen-track-artist track)))
             (list :name "Title" :max-width 35
                   :getter (lambda (track _table)
                             (listen-track-title track)))
             (list :name "Album" :max-width 30
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
       :sort-by '((1 . ascend))
       :actions `("q" (lambda (&rest _) (bury-buffer))
                  "n" (lambda (&rest _) (forward-line 1))
                  "p" (lambda (&rest _) (forward-line -1))
                  "RET" ,(listen-queue-command listen-queue-play)))
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      (re-search-forward "▶" nil t)
      (hl-line-mode 1))))

(defun listen-queue--update-buffer (queue)
  "Update QUEUE's buffer, if any."
  (when-let ((buffer (cl-loop for buffer in (buffer-list)
                              when (eq queue (buffer-local-value 'listen-queue buffer))
                              return buffer)))
    (with-current-buffer buffer
      (vtable-revert-command))))

(declare-function listen-play "listen")
(defun listen-queue-play (queue track)
  (let ((player (listen--player)))
    (listen-play player (listen-track-filename track))
    (setf (listen-queue-current queue) track
          (map-elt (listen-player-etc player) :queue) queue)
    (listen-queue--update-buffer queue)))

(defun listen-queue--format-time (time)
  "Return TIME formatted according to `listen-queue-time-format', which see."
  (if time
      (format-time-string listen-queue-time-format time)
    "never"))

(cl-defun listen-queue-complete (&key (prompt "Queue: "))
  "Return a Listen queue selected with completion.
PROMPT is passed to `completing-read', which see."
  (pcase (length listen-queues)
    (0 (call-interactively #'listen-queue-new))
    (1 (car listen-queues))
    (_ (let* ((queue-names (mapcar #'listen-queue-name listen-queues))
              (selected (completing-read prompt queue-names)))
         (if (member selected queue-names)
             (cl-find selected listen-queues :key #'listen-queue-name :test #'equal)
           (push (make-listen-queue :name selected) listen-queues))))))

;;;###autoload
(defun listen-queue-new (name)
  "Add and return a new queue having NAME."
  (interactive (list (read-string "New queue name: ")))
  (push (make-listen-queue :name name) listen-queues))

(defun listen-queue-discard (queue)
  "Discard QUEUE."
  (interactive (list (listen-queue-complete :prompt "Discard queue: ")))
  (cl-callf2 delete queue listen-queues))

;;;###autoload
(cl-defun listen-queue-add (queue files)
  "Add FILES to QUEUE."
  (interactive
   (let ((queue (listen-queue-complete))
         (path (expand-file-name (read-file-name "Enqueue file/directory: " listen-directory nil t))))
     (list queue
           (if (file-directory-p path)
               (directory-files-recursively path ".")
             (list path)))))
  (cl-callf append (listen-queue-tracks queue) (delq nil (mapcar #'listen-queue-track files))))

(defun listen-queue-track (filename)
  "Return track for FILENAME."
  (when-let ((metadata (emms-info-native--decode-info-fields filename)))
    (make-listen-track :filename filename
                       :artist (map-elt metadata "artist")
                       :title (map-elt metadata "title")
                       :album (map-elt metadata "album")
                       :number (map-elt metadata "tracknumber")
                       :date (map-elt metadata "date")
                       :genre (map-elt metadata "genre"))))

(defun listen-queue-next (queue)
  "Return QUEUE's next track after current."
  (seq-elt (listen-queue-tracks queue)
           (1+ (seq-position (listen-queue-tracks queue)
                             (listen-queue-current queue)))))

(provide 'listen-queue)
;;; listen-queue.el ends here
