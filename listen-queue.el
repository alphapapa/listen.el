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

;; FIXME: A track may be present in a queue multiple times, but the
;; commands don't operate on positions but the first instance of a
;; track found in the queue.

(require 'map)
(require 'ring)
(require 'vtable)

(require 'persist)

(require 'listen-lib)
(require 'listen-info)

(persist-defvar listen-queues nil
  "Listen queues.")

(defvar listen-directory)

(defvar-local listen-queue nil
  "Queue in this buffer.")

(defvar-local listen-queue-overlay nil)

(defvar-local listen-queue-kill-ring (make-ring 16)
  "Killed tracks.")

(defgroup listen-queue nil
  "Queues."
  :group 'listen)

;; (defmacro listen-queue-command (command)
;;   "Expand to a lambda that applies its args to COMMAND and reverts the list buffer."
;;   `(lambda (&rest args)
;;      (let ((list-buffer (current-buffer)))
;;        (apply #',command queue args)
;;        (with-current-buffer list-buffer
;;          (vtable-revert)))))

(declare-function listen-pause "listen")
;;;###autoload
(defun listen-queue (queue)
  "Show listen QUEUE."
  (interactive (list (listen-queue-complete)))
  (let ((buffer (get-buffer-create (format "*Listen Queue: %s*" (listen-queue-name queue)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setf listen-queue queue)
        (read-only-mode)
        (erase-buffer)
        (toggle-truncate-lines 1)
        (setq-local bookmark-make-record-function #'listen-queue--bookmark-make-record)
        (make-vtable
         :columns
         (list (list :name "*" :primary 'descend
                     :getter (lambda (track _table)
                               (if (eq track (listen-queue-current queue))
                                   "▶" " ")))
               (list :name "At" :primary 'descend
                     :getter (lambda (track _table)
                               (cl-position track (listen-queue-tracks queue))))
               (list :name "Artist" :max-width 20 :align 'right
                     :getter (lambda (track _table)
                               (propertize (or (listen-track-artist track) "")
                                           'face 'font-lock-variable-name-face)))
               (list :name "Title" :max-width 35
                     :getter (lambda (track _table)
                               (propertize (or (listen-track-title track) "")
                                           'face 'font-lock-function-name-face)))
               (list :name "Album" :max-width 30
                     :getter (lambda (track _table)
                               (propertize (or (listen-track-album track) "")
                                           'face 'font-lock-type-face)))
               (list :name "#"
                     :getter (lambda (track _table)
                               (or (listen-track-number track) "")))
               (list :name "Date"
                     :getter (lambda (track _table)
                               (or (listen-track-date track) "")))
               (list :name "Genre"
                     :getter (lambda (track _table)
                               (or (listen-track-genre track) "")))
               (list :name "File"
                     :getter (lambda (track _table)
                               (listen-track-filename track))))
         :objects-function (lambda ()
                             (or (listen-queue-tracks listen-queue)
                                 (list (make-listen-track :artist "[Empty queue]"))))
         :sort-by '((1 . ascend))
         :actions (list "q" (lambda (_) (bury-buffer))
                        "n" (lambda (_) (forward-line 1))
                        "p" (lambda (_) (forward-line -1))
                        "N" (lambda (track) (listen-queue-transpose-forward track queue))
                        "P" (lambda (track) (listen-queue-transpose-backward track queue))
                        "C-k" (lambda (track) (listen-queue-kill-track track queue))
                        "C-y" (lambda (_) (call-interactively #'listen-queue-yank))
                        "RET" (lambda (track) (listen-queue-play queue track))
                        "SPC" (lambda (_) (call-interactively #'listen-pause))
                        "S" (lambda (_) (listen-queue-shuffle listen-queue))))
        (goto-char (point-min))
        (listen-queue--highlight-current)
        (hl-line-mode 1)))
    (pop-to-buffer buffer)))

(cl-defun listen-queue-transpose-forward (track queue &key backwardp)
  "Transpose TRACK forward in QUEUE.
If BACKWARDP, move it backward."
  (interactive)
  (let* ((fn (if backwardp #'1- #'1+))
         (position (seq-position (listen-queue-tracks queue) track))
         (_ (when (= (funcall fn position) (length (listen-queue-tracks queue)))
              (user-error "Track at end of queue")))
         (next-position (funcall fn position))
         (next-track (seq-elt (listen-queue-tracks queue) next-position)))
    (setf (seq-elt (listen-queue-tracks queue) next-position) track
          (seq-elt (listen-queue-tracks queue) position) next-track)
    (listen-queue--update-buffer queue)))

(cl-defun listen-queue-transpose-backward (track queue)
  "Transpose TRACK backward in QUEUE."
  (interactive)
  (listen-queue-transpose-forward track queue :backwardp t))

(defun listen-queue-kill-track (track queue)
  "Remove TRACK from QUEUE."
  (interactive)
  (ring-insert listen-queue-kill-ring track)
  (cl-callf2 remove track (listen-queue-tracks queue))
  (listen-queue--update-buffer queue))

(defun listen-queue-yank (track position queue)
  "Yank TRACK into QUEUE at POSITION."
  (interactive
   (list (ring-ref listen-queue-kill-ring 0)
         (seq-position (listen-queue-tracks listen-queue) (vtable-current-object))
         listen-queue))
  (setf (listen-queue-tracks queue)
        (nconc (seq-take (listen-queue-tracks queue) position)
               (list track)
               (seq-subseq (listen-queue-tracks queue) position)))
  (listen-queue--update-buffer queue))

(defun listen-queue--highlight-current ()
  "Draw highlight onto current track."
  (when listen-queue-overlay
    (delete-overlay listen-queue-overlay))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "▶" nil t)
      (setf listen-queue-overlay (make-overlay (pos-bol) (pos-eol)))
      (overlay-put listen-queue-overlay 'face 'highlight))))

(defun listen-queue--update-buffer (queue)
  "Update QUEUE's buffer, if any."
  (when-let ((buffer (cl-loop for buffer in (buffer-list)
                              when (eq queue (buffer-local-value 'listen-queue buffer))
                              return buffer)))
    (with-current-buffer buffer
      (vtable-revert-command)
      (listen-queue--highlight-current))))

(declare-function listen-mode "listen")
(declare-function listen-play "listen")
(cl-defun listen-queue-play (queue &optional (track (car (listen-queue-tracks queue))))
  "Play QUEUE and optionally TRACK in it.
Interactively, selected queue with completion; and with prefix,
select track as well."
  (interactive
   (let* ((queue (listen-queue-complete))
          (track (if current-prefix-arg
                     (listen-queue-complete-track queue)
                   (car (listen-queue-tracks queue)))))
     (list queue track)))
  (let ((player (listen--player)))
    (listen-play player (listen-track-filename track))
    (setf (listen-queue-current queue) track
          (map-elt (listen-player-etc player) :queue) queue)
    (listen-queue--update-buffer queue))
  (listen-mode 1)
  queue)

(defun listen-queue-complete-track (queue)
  "Return track selected from QUEUE with completion."
  (cl-labels ((format-track (track)
                (pcase-let (((cl-struct listen-track artist title album date) track))
                  (format "%s: %s (%s) (%s)"
                          artist title album date))))
    (let* ((map (mapcar (lambda (track)
                          (cons (format-track track) track))
                        (listen-queue-tracks queue)))
           (selected (completing-read (format "Track from %S: " (listen-queue-name queue))
                                      map nil t)))
      (alist-get selected map nil nil #'equal))))

(declare-function listen--playing-p "listen-vlc")
(cl-defun listen-queue-complete (&key (prompt "Queue"))
  "Return a Listen queue selected with completion.
PROMPT is passed to `format-prompt', which see."
  (pcase (length listen-queues)
    (0 (call-interactively #'listen-queue-new))
    (1 (car listen-queues))
    (_ (let* ((player (listen--player))
              (default-queue-name (or (when listen-queue
                                        (listen-queue-name listen-queue))
                                      (when (listen--playing-p player)
                                        (listen-queue-name (map-elt (listen-player-etc player) :queue)))))
              (queue-names (mapcar #'listen-queue-name listen-queues))
              (prompt (format-prompt prompt default-queue-name))
              (selected (completing-read prompt queue-names nil t nil nil default-queue-name)))
         (cl-find selected listen-queues :key #'listen-queue-name :test #'equal)))))

;;;###autoload
(defun listen-queue-new (name)
  "Add and return a new queue having NAME."
  (interactive (list (read-string "New queue name: ")))
  (when (cl-find name listen-queues :key #'listen-queue-name :test #'equal)
    (user-error "Queue named %S already exists" name))
  (let ((queue (make-listen-queue :name name)))
    (push queue listen-queues)
    (listen-queue queue)
    queue))

(defun listen-queue-discard (queue)
  "Discard QUEUE."
  (interactive (list (listen-queue-complete :prompt "Discard queue: ")))
  (cl-callf2 delete queue listen-queues))

;;;###autoload
(cl-defun listen-queue-add-files (files queue)
  "Add FILES to QUEUE."
  (interactive
   (let ((queue (listen-queue-complete))
         (path (expand-file-name (read-file-name "Enqueue file/directory: " listen-directory nil t))))
     (list (if (file-directory-p path)
               (directory-files-recursively path ".")
             (list path))
           queue)))
  (cl-callf append (listen-queue-tracks queue) (delq nil (mapcar #'listen-queue-track files)))
  (listen-queue--update-buffer queue)
  queue)

(declare-function listen-mpd-completing-read "listen-mpd")
(cl-defun listen-queue-add-from-mpd (queue)
  "Add tracks to QUEUE selected from MPD library."
  (interactive (list (listen-queue-complete)))
  (require 'listen-mpd)
  (listen-queue-add-files (listen-mpd-completing-read :select-tag-p t) queue))

(defun listen-queue-track (filename)
  "Return track for FILENAME."
  (when-let ((metadata (listen-info--decode-info-fields filename)))
    (cl-assert metadata nil "Track has no metadata: %S" filename)
    (make-listen-track
     ;; Abbreviate the filename so as to not include the user's
     ;; homedir path (so queues could be portable with music
     ;; libraries).
     :filename (abbreviate-file-name filename)
     :artist (map-elt metadata "artist")
     :title (map-elt metadata "title")
     :album (map-elt metadata "album")
     :number (map-elt metadata "tracknumber")
     :date (map-elt metadata "date")
     :genre (map-elt metadata "genre"))))

(defun listen-queue-shuffle (queue)
  "Shuffle QUEUE."
  (interactive (list (listen-queue-complete)))
  ;; Copied from `elfeed-shuffle'.
  (let* ((tracks (listen-queue-tracks queue))
         (current-track (listen-queue-current queue))
         n)
    (when current-track
      (cl-callf2 delete current-track tracks))
    (setf n (length tracks))
    ;; Don't use dotimes result (bug#16206)
    (dotimes (i n)
      (cl-rotatef (elt tracks i) (elt tracks (+ i (cl-random (- n i))))))
    (when current-track
      (push current-track tracks))
    (setf (listen-queue-tracks queue) tracks))
  (listen-queue--update-buffer queue))

(defun listen-queue-next (queue)
  "Play next track in QUEUE."
  (interactive (list (listen-queue-complete)))
  (listen-queue-play queue (listen-queue-next-track queue)))

(defun listen-queue-next-track (queue)
  "Return QUEUE's next track after current."
  (seq-elt (listen-queue-tracks queue)
           (1+ (seq-position (listen-queue-tracks queue)
                             (listen-queue-current queue)))))

;;;;; Bookmark support

(require 'bookmark)

(defun listen-queue--bookmark-make-record ()
  "Return a bookmark record for the current queue buffer."
  (cl-assert listen-queue)
  `(,(format "Listen: %s" (listen-queue-name listen-queue))
    (handler . listen-queue--bookmark-handler)
    (queue-name . ,(listen-queue-name listen-queue))))

(defun listen-queue--bookmark-handler (bookmark)
  "Set current buffer to BOOKMARK's listen queue."
  (let* ((queue-name (bookmark-prop-get bookmark 'queue-name))
         (queue (cl-find queue-name listen-queues :key #'listen-queue-name :test #'equal)))
    (unless queue
      (error "No Listen queue found named %S" queue-name))
    (listen-queue queue)))

;;;; Footer

(provide 'listen-queue)

;;; listen-queue.el ends here
