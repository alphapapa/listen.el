;;; listen-queue.el --- Listen queue                 -*- lexical-binding: t; -*-

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

(defvar listen-mode)

(defvar listen-queue-ffprobe-p (not (not (executable-find "ffprobe")))
  "Whether \"ffprobe\" is available.")

(defvar listen-queue-nice-p (not (not (executable-find "nice")))
  "Whether \"nice\" is available.")

(defgroup listen-queue nil
  "Queues."
  :group 'listen)

(defcustom listen-queue-max-probe-processes (max 1 (/ (num-processors) 2))
  "Maximum number of processes to run while probing track durations."
  :type 'natnum)

(defcustom listen-queue-repeat-mode nil
  "Whether and how to repeat queues.
This is provided as a customization option, but it's mainly
intended to be set from the `listen-menu'."
  :type '(choice (const :tag "Don't repeat" nil)
                 (const :tag "Repeat queue" queue)
                 (const :tag "Shuffle and repeat queue"
                        :documentation "When the queue finishes, shuffle it and play again."
                        shuffle)
                 ;; TODO: Implement track repeat (it doesn't fit into the current logic).
                 ;; (const :tag "Repeat track" track)
                 ))

;;;; Macros

(defmacro listen-queue-with-buffer (queue &rest body)
  "Eval BODY in QUEUE's buffer, if it has a live one."
  (declare (indent defun) (debug (sexp body)))
  (let ((buffer-var (gensym)))
    `(when-let ((,buffer-var (listen-queue-buffer ,queue)))
       (when (buffer-live-p ,buffer-var)
         (with-current-buffer ,buffer-var
           ,@body)))))

(defmacro listen-save-position (&rest body)
  "Eval BODY and go to previous position.
Useful for when `save-excursion' does not preserve point."
  (declare (indent defun) (debug (body)))
  (let ((pos-var (gensym)))
    `(let ((,pos-var (point)))
       ,@body
       ;; Ignore errors in case it's now out of range.
       (ignore-errors
         (goto-char ,pos-var)))))

;;;; Commands

;; (defmacro listen-queue-command (command)
;;   "Expand to a lambda that applies its args to COMMAND and reverts the list buffer."
;;   `(lambda (&rest args)
;;      (let ((list-buffer (current-buffer)))
;;        (apply #',command queue args)
;;        (with-current-buffer list-buffer
;;          (vtable-revert)))))

(declare-function listen-jump "listen")
(declare-function listen-menu "listen")
(declare-function listen-pause "listen")

(define-derived-mode listen-queue-mode special-mode "Listen-Queue"
  (toggle-truncate-lines 1)
  (hl-line-mode 1)
  (setq-local bookmark-make-record-function #'listen-queue--bookmark-make-record
              mode-name
              '("Listen-Queue"
                (:eval (when (and listen-player
                                  (eq listen-queue (alist-get :queue (listen-player-etc listen-player))))
                         (propertize ":current" 'face 'font-lock-builtin-face))))))

;;;###autoload
(defun listen-queue (queue)
  "Show listen QUEUE."
  (interactive (list (listen-queue-complete)))
  (if-let ((buffer (listen-queue-buffer queue)))
      (progn
        (pop-to-buffer buffer)
        (listen-queue-goto-current queue))
    (with-current-buffer
        (setf buffer (get-buffer-create (format "*Listen Queue: %s*" (listen-queue-name queue))))
      (let ((inhibit-read-only t))
        (listen-queue-mode)
        (setf listen-queue queue)
        (erase-buffer)
        (when (listen-queue-tracks listen-queue)
          (listen-make-vtable
           :columns
           (list (list :name "▶" :primary 'descend
                       :getter
                       (lambda (track _table)
                         ;; We compare filenames in case the queue's files
                         ;; have been refreshed from disk, in which case
                         ;; the track objects would no longer be `eq'.
                         (if-let ((player listen-player)
                                  ((eq queue (alist-get :queue (listen-player-etc player))))
                                  (current-track (listen-queue-current queue))
                                  ((equal (listen-track-filename track)
                                          (listen-track-filename current-track))))
                             (progn
                               (unless (eq track (listen-queue-current listen-queue))
                                 (if-let ((position (seq-position (listen-queue-tracks listen-queue)
                                                                  (listen-queue-current listen-queue))))
                                     ;; HACK: Update current track in queue.  I don't know a more
                                     ;; optimal place to do this.
                                     ;; TODO: Potentially use `listen-queue-revert-track' in more
                                     ;; places to make this unnecessary.
                                     (setf (seq-elt (listen-queue-tracks listen-queue) position) track)
                                   ;; Old track not found: just add it.
                                   (push track (listen-queue-tracks queue)))
                                 (setf (listen-queue-current listen-queue) track))
                               "▶")
                           " ")))
                 (list :name "#" :primary 'descend
                       :getter (lambda (track _table)
                                 (cl-position track (listen-queue-tracks queue))))
                 (list :name "Duration"
                       :getter (lambda (track _table)
                                 (when-let ((duration (listen-track-duration track)))
                                   (listen-format-seconds duration))))
                 (list :name "r/5"
                       :getter (lambda (track _table)
                                 (if-let ((rating (map-elt (listen-track-etc track) "fmps_rating"))
                                          ((not (equal "-1" rating))))
                                     (progn
                                       (setf rating (number-to-string (* 5 (string-to-number rating))))
                                       (propertize rating 'face 'listen-rating))
                                   "")))
                 (list :name "Artist" :max-width 20 :align 'right
                       :getter (lambda (track _table)
                                 (propertize (or (listen-track-artist track) "")
                                             'face 'listen-artist)))
                 (list :name "Title" :max-width 35
                       :getter (lambda (track _table)
                                 (propertize (or (listen-track-title track) "")
                                             'face 'listen-title)))
                 (list :name "Album" :max-width 30
                       :getter (lambda (track _table)
                                 (propertize (or (listen-track-album track) "")
                                             'face 'listen-album)))
                 (list :name "#" :align 'right
                       :getter (lambda (track _table)
                                 (or (when-let ((number (listen-track-number track)))
                                       (string-to-number number))
                                     "")))
                 (list :name "Date"
                       :getter (lambda (track _table)
                                 (or (map-elt (listen-track-etc track) "originalyear")
                                     (map-elt (listen-track-etc track) "originaldate")
                                     (listen-track-date track)
                                     "")))
                 (list :name "Genre"
                       :getter (lambda (track _table)
                                 (propertize (or (listen-track-genre track) "")
                                             'face 'listen-genre)))
                 (list :name "File"
                       :getter (lambda (track _table)
                                 (propertize (listen-track-filename track)
                                             'face 'listen-filename))))
           :objects-function (lambda ()
                               (or (listen-queue-tracks listen-queue)
                                   (list (make-listen-track :artist "[Empty queue]"))))
           :sort-by '((1 . ascend))
           ;; TODO: Add a transient to show these bindings when pressing "?".
           :actions (list "q" (lambda (_) (bury-buffer))
                          "?" (lambda (_) (call-interactively #'listen-menu))
                          "g" (lambda (_) (call-interactively #'listen-queue-revert))
                          "j" #'listen-jump
                          "n" (lambda (_) (forward-line 1))
                          "p" (lambda (_) (forward-line -1))
                          "m" #'listen-view-track
                          "N" (lambda (track) (listen-queue-transpose-forward track queue))
                          "P" (lambda (track) (listen-queue-transpose-backward track queue))
                          "C-k" (lambda (track) (listen-queue-kill-track track queue))
                          "C-y" (lambda (_) (call-interactively #'listen-queue-yank))
                          "RET" (lambda (track) (listen-queue-play queue track))
                          "SPC" (lambda (_) (call-interactively #'listen-pause))
                          "o" (lambda (_) (call-interactively #'listen-queue-order-by))
                          "s" (lambda (_) (listen-queue-shuffle listen-queue))
                          "l" (lambda (_) "Show (selected) tracks in library view."
                                (call-interactively #'listen-library-from-queue))
                          "!" (lambda (_) (call-interactively #'listen-queue-shell-command)))))
        (listen-queue--annotate-buffer)
        (listen-queue-goto-current queue)))
    ;; NOTE: We pop to the buffer outside of `with-current-buffer' so
    ;; `listen-queue--bookmark-handler' works correctly.
    (pop-to-buffer buffer)))

(defun listen-queue--annotate-buffer ()
  "Annotate current buffer.
To be called in a queue's buffer."
  (let* ((queue listen-queue)
         ;; HACK: Update duration here (for now).
         (duration (cl-reduce #'+ (listen-queue-tracks queue)
                              :key #'listen-track-duration))
         (inhibit-read-only t))
    (setf (map-elt (listen-queue-etc queue) :duration) duration)
    (vtable-end-of-table)
    (delete-region (point) (point-max))
    (when duration
      (insert (format "Duration: %s" (listen-format-seconds duration))))
    (listen-queue--highlight-current)))

(defun listen-queue--highlight-current ()
  "Draw highlight onto current track."
  (when listen-queue-overlay
    (delete-overlay listen-queue-overlay))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "▶" nil t)
      (setf listen-queue-overlay (make-overlay (pos-bol) (pos-eol)))
      (overlay-put listen-queue-overlay 'face 'highlight))))

(cl-defun listen-queue-transpose-forward (track queue &key backwardp)
  "Transpose TRACK forward in QUEUE.
If BACKWARDP, move it backward."
  (interactive)
  (let* ((fn (if backwardp #'1- #'1+))
         (position (seq-position (listen-queue-tracks queue) track))
         (_ (when (= (funcall fn position) (length (listen-queue-tracks queue)))
              (user-error "Track at end of queue")))
         (next-position (funcall fn position)))
    ;; Hey, a chance to use `rotatef'!
    (cl-rotatef (seq-elt (listen-queue-tracks queue) next-position)
                (seq-elt (listen-queue-tracks queue) position))
    ;; TODO: Use `vtable-insert-object' and `vtable-remove-object' to avoid calling
    ;; `listen-queue--update-buffer'.
    (listen-queue--update-buffer queue)
    (vtable-goto-object track)))

(cl-defun listen-queue-transpose-backward (track queue)
  "Transpose TRACK backward in QUEUE."
  (interactive)
  (listen-queue-transpose-forward track queue :backwardp t))

(defun listen-queue-kill-track (track queue)
  "Remove TRACK from QUEUE."
  (interactive)
  (ring-insert listen-queue-kill-ring track)
  (cl-callf2 remove track (listen-queue-tracks queue))
  (listen-save-position
    (goto-char (point-min))
    (vtable-goto-object track)
    ;; NOTE: We don't update the subsequent tracks, which means their position number will be off,
    ;; though still in order.  This is because in large queues (e.g. 2k tracks), refreshing the
    ;; whole vtable, or even the rest of it, is too slow.
    (vtable-remove-object (vtable-current-table) track)))

(defun listen-queue-yank (track position queue)
  "Yank TRACK into QUEUE at POSITION."
  (interactive
   (list (ring-ref listen-queue-kill-ring 0)
         (seq-position (listen-queue-tracks listen-queue) (vtable-current-object))
         listen-queue))
  (let ((previous-track (seq-elt (listen-queue-tracks queue) position)))
    (setf (listen-queue-tracks queue)
          (nconc (seq-take (listen-queue-tracks queue) position)
                 (list track)
                 (seq-subseq (listen-queue-tracks queue) position)))
    (vtable-insert-object (vtable-current-table) track previous-track)
    (listen-queue--vtable-update-object (vtable-current-table) previous-track previous-track)))

(defun listen-queue--update-buffer (queue)
  "Update QUEUE's buffer, if any."
  (listen-queue-with-buffer queue
    ;; `save-excursion' doesn't work because of the table's being reverted.
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (vtable-current-table)
        (vtable-revert-command))
      (listen-queue--annotate-buffer))
    (listen-queue-goto-current queue)))

(defun listen-queue-goto-current (queue)
  "Jump to current track."
  (interactive (list (listen-queue-complete)))
  (unless (listen-queue-buffer queue)
    (listen-queue queue))
  (listen-queue-with-buffer queue
    (when-let ((current-track (listen-queue-current queue)))
      ;; Ensure point is within the vtable.
      (goto-char (point-min))
      (vtable-goto-object current-track))
    (unless (get-buffer-window (current-buffer))
      (display-buffer (current-buffer)))))

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
(cl-defun listen-queue-complete (&key (prompt "Queue") allow-new-p)
  "Return a Listen queue selected with completion.
If ALLOW-NEW-P, accept the name of a non-existent queue and
return a new one having it.  PROMPT is passed to `format-prompt',
which see."
  (cl-labels ((read-queue ()
                (let* ((player (listen-current-player))
                       (default-queue-name
                        (when-let ((queue (or listen-queue
                                              (when (listen--playing-p player)
                                                (map-elt (listen-player-etc player) :queue)))))
                          (listen-queue-name queue)))
                       (queue-names (mapcar #'listen-queue-name listen-queues))
                       (prompt (format-prompt prompt default-queue-name))
                       (selected (completing-read prompt queue-names nil (not allow-new-p)
                                                  nil nil default-queue-name)))
                  (or (cl-find selected listen-queues :key #'listen-queue-name :test #'equal)
                      (when allow-new-p
                        (listen-queue--new selected))))))
    (pcase (length listen-queues)
      (0 (listen-queue--new (read-string "New queue name: ")))
      (_ (read-queue)))))

;;;###autoload
(defun listen-queue-new (name)
  "Add and show a new queue having NAME."
  (interactive (list (read-string "New queue name: ")))
  (let ((queue (listen-queue--new name)))
    (listen-queue queue)
    queue))

(defun listen-queue--new (name)
  "Add and return a new queue having NAME."
  (when (cl-find name listen-queues :key #'listen-queue-name :test #'equal)
    (user-error "Queue named %S already exists" name))
  (let ((queue (make-listen-queue :name name)))
    (push queue listen-queues)
    queue))

(defun listen-queue-discard (queue)
  "Discard QUEUE."
  (interactive (list (listen-queue-complete :prompt "Discard queue: ")))
  (cl-callf2 delete queue listen-queues))

(defun listen-complete-files ()
  ;; FIXME: Use this function in more places as appropriate.
  "Return files selected with completion.
In a Dired buffer, use `dired-get-marked-files'."
  (declare-function dired-get-marked-files "dired")
  (cl-case major-mode
    (dired-mode (dired-get-marked-files))
    (otherwise (let ((path (expand-file-name (read-file-name "Enqueue file/directory: "
                                                             listen-directory nil t))))
                 (if (file-directory-p path)
                     (directory-files-recursively path ".")
                   (list path))))))

;;;###autoload
(cl-defun listen-queue-add-files (files queue)
  "Add FILES to QUEUE.
Completes files with `listen-complete-files', which see."
  (interactive
   (let ((queue (listen-queue-complete :allow-new-p t)))
     (list (listen-complete-files) queue)))
  (cl-callf append (listen-queue-tracks queue) (listen-queue-tracks-for files))
  (listen-queue queue)
  queue)

(defun listen-queue-add-tracks (tracks queue)
  "Add TRACKS to QUEUE.
Duplicate tracks (by filename) are removed from the queue, and
the queue's buffer is updated, if any."
  (cl-callf append (listen-queue-tracks queue) tracks)
  ;; TODO: Consider updating the metadata of any duplicate tracks.
  (setf (listen-queue-tracks queue)
        (listen-delete-dups (listen-queue-tracks queue) 'listen-track-equal))
  (listen-queue--update-buffer queue))

(cl-defun listen-queue-add-from-playlist-file (filename queue)
  "Add tracks to QUEUE selected from playlist at FILENAME.
M3U playlists are supported."
  (interactive
   (let ((filename
          (read-file-name "Add tracks from playlist: " listen-directory nil t nil
                          (lambda (filename)
                            (pcase (file-name-extension filename)
                              ("m3u" t)))))
         (queue (listen-queue-complete :allow-new-p t)))
     (list filename queue)))
  (listen-queue-add-files (listen-queue--m3u-filenames filename) queue))

(defun listen-queue-buffer (queue)
  "Return QUEUE's buffer, if any."
  (cl-loop for buffer in (buffer-list)
           when (eq queue (buffer-local-value 'listen-queue buffer))
           return buffer))

(declare-function listen-library "listen-library")
(cl-defun listen-library-from-queue (&key tracks queue)
  "Display TRACKS from QUEUE in library view.
Interactively, use tracks from QUEUE (or selected ones in its
buffer, if any)."
  (interactive
   (let* ((queue (if (and listen-queue (use-region-p))
                     ;; In a queue buffer and the region is active: use it.
                     listen-queue
                   (listen-queue-complete :allow-new-p t)))
          (tracks (when-let ((buffer (listen-queue-buffer queue)))
                    (with-current-buffer buffer
                      (when (region-active-p)
                        (listen-queue-selected))))))
     (list :tracks tracks :queue queue)))
  (listen-library (or tracks
                      (lambda ()
                        (listen-queue-tracks
                         ;; In case the queue gets renamed, or gets replaced by a
                         ;; different one with the same name:
                         (or (when (member queue listen-queues)
                               ;; Ensure the queue is in the queue list (one from a bookmark
                               ;; record wouldn't be the same object anymore).  This allows
                               ;; a queue to be renamed during a session and still match
                               ;; here.
                               queue)
                             (cl-find (listen-queue-name queue) listen-queues
                                      :key #'listen-queue-name :test #'equal)
                             (error "Queue not found: %S" queue))))) ))

(defun listen-queue-track (filename)
  "Return track for FILENAME."
  (when-let ((metadata (listen-info--decode-info-fields filename)))
    ;; FIXME: This assertion.
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
     :genre (map-elt metadata "genre")
     :rating (map-elt metadata "fmps_rating")
     ;; TODO: Stop also storing metadata in etc slot.
     :etc metadata
     :metadata metadata)))

(defun listen-queue-tracks-for (filenames)
  "Return tracks for FILENAMES.
When `listen-queue-ffprobe-p' is non-nil, adds durations read
with \"ffprobe\"."
  (with-demoted-errors "listen-queue-tracks-for: %S"
    (let ((tracks (remq nil (mapcar #'listen-queue-track filenames))))
      (when listen-queue-ffprobe-p
        (listen-queue--add-track-durations tracks))
      tracks)))

(defun listen-queue-revert-track (track)
  "Revert TRACK's metadata from disk."
  ;; TODO: Use this where appropriate.
  (when-let ((new-track (car (listen-queue-tracks-for (list (listen-track-filename track))))))
    ;; If `listen-queue-track' (and thereby `listen-queue-tracks-for') returns nil for a track
    ;; (e.g. if its metadata can't be read), leave it alone (e.g. its metadata might have come from
    ;; by MPD).
    ;; FIXME: Store metadata in its own slot and don't misuse etc slot.
    (setf (listen-track-etc track) (listen-track-etc new-track)
          (listen-track-metadata track) (listen-track-etc new-track))
    (dolist (slot '(artist title album number date genre etc))
      (setf (cl-struct-slot-value 'listen-track slot track)
            (cl-struct-slot-value 'listen-track slot new-track)))))

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

(cl-defun listen-queue-deduplicate (queue)
  "Remove duplicate tracks from QUEUE.
Tracks that appear to have the same metadata (artist, album, and
title, compared case-insensitively) are deduplicated.  Also, any
tracks no longer backed by a file are removed."
  (interactive (list (listen-queue-complete)))
  ;; Remove any tracks with missing files first, so as not to remove
  ;; an apparent duplicate that does have a file.
  (setf (listen-queue-tracks queue)
        (cl-remove-if-not #'file-exists-p (listen-queue-tracks queue)
                          :key (lambda (track)
                                 (expand-file-name (listen-track-filename track))))
        (listen-queue-tracks queue)
        (cl-remove-duplicates
         (listen-queue-tracks queue)
         :test (lambda (a b)
                 (pcase-let ((( cl-struct listen-track
                                (artist a-artist) (album a-album) (title a-title)) a)
                             (( cl-struct listen-track
                                (artist b-artist) (album b-album) (title b-title)) b))
                   (and (or (and a-artist b-artist)
                            (and a-album b-album)
                            (and a-title b-title))
                        ;; Tracks have at least one common metadata field: compare them.
                        (if (and a-artist b-artist)
                            (string-equal-ignore-case a-artist b-artist)
                          t)
                        (if (and a-album b-album)
                            (string-equal-ignore-case a-album b-album)
                          t)
                        (if (and a-title b-title)
                            (string-equal-ignore-case a-title b-title)
                          t))))))
  (listen-queue--update-buffer queue))

(defun listen-queue-next (queue)
  "Play next track in QUEUE."
  (interactive (list (listen-queue-complete)))
  (listen-queue-play queue (listen-queue-next-track queue)))

(defun listen-queue-next-track (queue)
  "Return QUEUE's next track after current."
  (or (ignore-errors
        (seq-elt (listen-queue-tracks queue)
                 (1+ (seq-position (listen-queue-tracks queue)
                                   (listen-queue-current queue) #'eq))))
      ;; Couldn't find position of current track: maybe the track
      ;; object changed while it was playing (e.g. if the user changes
      ;; the track metadata and refreshes the queue from disk while
      ;; the track is playing), in which case it won't be able to find
      ;; the track in the queue, so look again by comparing filenames.
      (let ((current-track-position
             (or (seq-position (listen-queue-tracks queue)
                               (listen-queue-current queue)
                               (lambda (a b)
                                 (equal (expand-file-name (listen-track-filename a))
                                        (expand-file-name (listen-track-filename b)))))
                 (progn
                   (display-warning 'listen-queue
                                    (format-message "listen: Can't find track (%S) in queue (%S)"
                                                    (listen-queue-current queue)
                                                    (listen-queue-name queue))
                                    :debug)
                   (if-let ((track-number (map-elt (listen-queue-etc queue) :track-number)))
                       ;; If the track was removed, the next track in the queue should have
                       ;; taken its place, and we don't want to skip it, so subtract one.
                       (1- track-number)
                     (error "listen: Couldn't find track in queue, and track number is nil"))))))
        (seq-elt (listen-queue-tracks queue) (1+ current-track-position)))))

(declare-function listen-shell-command "listen")
(defun listen-queue-shell-command (command filenames)
  "Run COMMAND on FILENAMES.
Interactively, read COMMAND and use tracks at point in current
queue buffer."
  (interactive
   (let* ((filenames (mapcar #'listen-track-filename (listen-queue-selected)))
          (command (read-shell-command (format "Run command on %S: " filenames))))
     (list command filenames)))
  (listen-shell-command command filenames)
  ;; NOTE: This code below would be great but for using async shell
  ;; command in `listen-shell-command'.  Also, if the files end up
  ;; renamed, they'll not be found, but that's up to the user.
  ;; (seq-do (lambda (filename)
  ;;           (setf (seq-elt (listen-queue-tracks listen-queue)
  ;;                          (seq-position (listen-queue-tracks listen-queue) filename
  ;;                                        (lambda (track)
  ;;                                          (equal filename (listen-track-filename track)))))
  ;;                 (listen-queue-track filename)))
  ;;         filenames)
  ;; (listen-queue-revert)
  )

(defun listen-queue-rename (name queue)
  "Rename QUEUE to NAME."
  (interactive
   (let* ((queue (listen-queue-complete))
          (name (read-string (format "Rename queue %S:" (listen-queue-name queue)))))
     (list name queue)))
  (setf (listen-queue-name queue) name))

(cl-defun listen-queue-revert (queue &key reloadp)
  "Revert QUEUE's buffer.
When RELOADP (interactively, with prefix), reload tracks from
disk."
  ;; TODO: Revise the terminology (i.e. "revert" should mean to revert from disk).
  (interactive (list listen-queue :reloadp current-prefix-arg))
  (when reloadp
    (listen-queue-reload queue)
    (when (listen-queue-current queue)
      ;; Update current track by filename.
      (setf (listen-queue-current queue)
            (cl-find (listen-track-filename (listen-queue-current queue))
                     (listen-queue-tracks queue) :key #'listen-track-filename :test #'file-equal-p))))
  (listen-queue--update-buffer queue))

(defun listen-queue-reload (queue)
  "Reload QUEUE's tracks from disk."
  (mapc #'listen-queue-revert-track (listen-queue-tracks queue)))

(defun listen-queue-order-by ()
  "Order the queue by the column at point.
That is, set the order of the tracks in the queue to match their
order in the view after sorting by the column at point (whereas
merely sorting the view by a column leaves the order of the
tracks in the queue unchanged)."
  (interactive)
  (vtable-sort-by-current-column)
  (save-excursion
    (goto-char (point-min))
    (setf (listen-queue-tracks listen-queue)
          (cl-loop for track = (ignore-errors (vtable-current-object))
                   while track
                   collect track
                   do (forward-line 1))))
  (listen-queue-revert listen-queue))

(defun listen-queue-selected ()
  "Return tracks selected in current queue buffer."
  (cl-assert listen-queue)
  (if (not (region-active-p))
      (list (vtable-current-object))
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char beg)
        (cl-loop collect (vtable-current-object)
                 do (forward-line 1)
                 while (<= (point) end))))))

(cl-defun listen-queue-remaining-duration (&optional (player (listen-current-player)))
  "Return seconds remaining in PLAYER's queue."
  (when-let ((queue (map-elt (listen-player-etc player) :queue))
             (current-track-remaining (- (listen--length player) (listen--elapsed player)))
             (current-track-position (cl-position (listen-queue-current queue)
                                                  (listen-queue-tracks queue)))
             (remaining-tracks (cl-subseq (listen-queue-tracks queue) (1+ current-track-position)))
             (remaining-tracks-duration (cl-reduce #'+ remaining-tracks :key #'listen-track-duration)))
    (+ current-track-remaining remaining-tracks-duration)))

(cl-defun listen-queue-format-remaining (&optional (player (listen-current-player)))
  "Return PLAYER's queue's remaining duration formatted."
  (when-let ((duration (listen-queue-remaining-duration player)))
    (concat "-" (listen-format-seconds duration))))

;;;;; Track view

;;;###autoload
(defun listen-view-track (track)
  "View information about TRACK."
  (interactive (list (listen-queue-current (map-elt (listen-player-etc (listen-current-player)) :queue))))
  (with-current-buffer (get-buffer-create (format "*Listen track: %S*" (listen-track-filename track)))
    (let ((inhibit-read-only t))
      (read-only-mode)
      (erase-buffer)
      (toggle-truncate-lines 1)
      (cl-labels ((get (slot)
                    (cons (capitalize (symbol-name slot))
                          (cl-struct-slot-value 'listen-track slot track))))
        (listen-make-vtable
         :columns
         (list (list :name "Key" :getter (lambda (row _table) (car row)))
               (list :name "Value" :getter (lambda (row _table) (cdr row))))
         :objects-function
         (lambda ()
           (append (list (get 'filename)
                         (get 'artist)
                         (get 'title)
                         (get 'album)
                         (get 'number)
                         (get 'date)
                         (cons " " " "))
                   (sort (listen-info--decode-info-fields (listen-track-filename track))
                         (lambda (a b)
                           (string< (car a) (car b))))))
         :actions (list "q" (lambda (_) (quit-window))
                        "g" (lambda (_)
                              (listen-queue-revert-track track)
                              (vtable-revert-command))
                        "?" (lambda (_) (call-interactively #'listen-menu))
                        "n" (lambda (_) (forward-line 1))
                        "p" (lambda (_) (forward-line -1))
                        "SPC" (lambda (_) (call-interactively #'listen-pause))
                        "!" (lambda (_) (let ((filename (listen-track-filename track)))
                                          (listen-shell-command
                                           (read-shell-command (format "Run command on %S: " filename))
                                           (list filename)))))))
      (goto-char (point-min))
      (hl-line-mode 1))
    (pop-to-buffer (current-buffer))))

;;;;; Bookmark support

(require 'bookmark)

(defun listen-queue--bookmark-make-record ()
  "Return a bookmark record for the current queue buffer."
  (cl-assert listen-queue)
  `(,(format "Listen: %s" (listen-queue-name listen-queue))
    (handler . listen-queue--bookmark-handler)
    (queue-name . ,(listen-queue-name listen-queue))))

;;;###autoload
(defun listen-queue--bookmark-handler (bookmark)
  "Set current buffer to BOOKMARK's listen queue."
  (let* ((queue-name (bookmark-prop-get bookmark 'queue-name))
         (queue (cl-find queue-name listen-queues :key #'listen-queue-name :test #'equal)))
    (unless queue
      (error "No Listen queue found named %S" queue-name))
    (listen-queue queue)))

(defun listen-queue-list--bookmark-make-record ()
  "Return a bookmark record for the `listen-queue-list' buffer."
  `("*Listen Queues*"
    (handler . ,#'listen-queue-list--bookmark-handler)))

;;;###autoload
(defun listen-queue-list--bookmark-handler (_bookmark)
  "Set current buffer to `listen-queue-list'."
  (listen-queue-list))

;;;;; M3U playlist support

(defun listen-queue--m3u-filenames (filename)
  "Return filenames from M3U playlist at FILENAME.
Expands filenames relative to playlist's directory."
  (let ((default-directory (file-name-directory filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (cl-loop while (re-search-forward (rx bol (group (not (any "#")) (1+ nonl)) eol) nil t)
               collect (expand-file-name (match-string 1))))))

;;;;; ffprobe queue

(cl-defun listen-queue--add-track-durations (tracks &key (max-processes listen-queue-max-probe-processes))
  "Add durations to TRACKS by probing with \"ffprobe\".
MAX-PROCESSES limits the number of parallel probing processes."
  ;; Because running "ffprobe" sequentially can be quite slow, we do
  ;; it asynchronously in a queue.
  ;; TODO: Generalize this.
  (let (processes)
    (cl-labels
        ((probe-duration (track)
           (with-demoted-errors "Unable to get duration for %S"
             (with-current-buffer (generate-new-buffer " *listen: ffprobe*")
               (let* ((sentinel (lambda (process status)
                                  (unwind-protect
                                      (pcase status
                                        ((or "killed\n" "interrupt\n"
                                             (pred numberp)
                                             (rx "exited abnormally with code " (1+ digit))))
                                        ("finished\n"
                                         (with-current-buffer (process-buffer process)
                                           (goto-char (point-min))
                                           (let ((duration (read (current-buffer))))
                                             (cl-check-type duration number)
                                             (setf (listen-track-duration track) duration)))))
                                    (kill-buffer (process-buffer process))
                                    (cl-callf2 remove process processes)
                                    (probe-more))))
                      (command (list "ffprobe" "-v" "quiet" "-print_format"
                                     "compact=print_section=0:nokey=1:escape=csv"
                                     "-show_entries" "format=duration"
                                     (expand-file-name (listen-track-filename track))))
                      (process (make-process
                                :name "listen:ffprobe" :noquery t :connection-type 'pipe
                                :buffer (current-buffer) :sentinel sentinel
                                :command (if listen-queue-nice-p
                                             (cons "nice" command)
                                           command))))
                 process))))
         (probe-more ()
           (while (and tracks (length< processes max-processes))
             (let ((track (pop tracks)))
               (push (probe-duration track) processes)))))
      (with-timeout ((max 0.2 (* 0.1 (length tracks)))
                     (error "Probing for track duration timed out"))
        (while (or tracks processes)
          (probe-more)
          (while (accept-process-output nil 0.01)))))))

;;;;; Queue delay mode

;; When you want music to play periodically, e.g. like Minecraft does.

(defvar listen-queue-delay-timer nil)

(defcustom listen-queue-delay-time-range '(120 . 600)
  "Range of delay in seconds."
  :type '(cons (natnum :tag "Minimum delay")
               (natnum :tag "Maximum delay")))

(declare-function listen-play-next "listen")
(define-minor-mode listen-queue-delay-mode
  "Delay playing the next track in the queue by a random amount of time.
Delay according to `listen-queue-delay-time-range', which see."
  :global t
  (if listen-queue-delay-mode
      (advice-add #'listen-play-next :around #'listen-queue-play-next-delayed)
    (advice-remove #'listen-play-next #'listen-queue-play-next-delayed)
    (when (timerp listen-queue-delay-timer)
      (setf listen-queue-delay-timer (cancel-timer listen-queue-delay-timer)))))

(defun listen-queue-play-next-delayed (oldfun player)
  "Call OLDFUN to play PLAYER's queue's next track after a random delay.
Delay according to `listen-queue-delay-time-range', which see."
  (when-let ((queue (map-elt (listen-player-etc player) :queue)))
    ;; Wrapping with `listen-once-per' protects against this function
    ;; being called multiple times while `listen-queue-delay-mode' is
    ;; enabled.  Sort of a hack, but it will serve until a refactor.
    (listen-once-per (listen-queue-next-track queue)
      ;; FIXME: Since `random' takes an upper limit, by having a floor
      ;; for values which are below the minimum, the delay is biased.
      (let ((delay-seconds (max (car listen-queue-delay-time-range)
                                (random (cdr listen-queue-delay-time-range)))))
        (setf listen-queue-delay-timer (run-at-time delay-seconds nil oldfun player))))))

;;;;; Queue list

(defun listen-queue-list ()
  "Show queue list."
  (interactive)
  (let ((buffer (get-buffer-create "*Listen Queues*"))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (read-only-mode)
      (erase-buffer)
      (toggle-truncate-lines 1)
      (setq-local bookmark-make-record-function #'listen-queue-list--bookmark-make-record)
      (when listen-queues
        (listen-make-vtable
         :columns
         (list (list :name "▶" :primary 'descend
                     :getter (lambda (queue _table)
                               (if-let ((player listen-player)
                                        ((eq queue (alist-get :queue (listen-player-etc player)))))
                                   "▶"
                                 " ")))
               (list :name "Name" :primary 'ascend
                     :getter (lambda (queue _table)
                               (listen-queue-name queue)))
               (list :name "Tracks" :align 'right
                     :getter (lambda (queue _table)
                               (length (listen-queue-tracks queue))))
               (list :name "Duration" :align 'right
                     :getter (lambda (queue _table)
                               (when-let ((duration (alist-get :duration (listen-queue-etc queue))))
                                 (listen-format-seconds duration)))))
         :objects-function (lambda ()
                             listen-queues)
         :sort-by '((1 . ascend))
         ;; TODO: Add a transient to show these bindings when pressing "?".
         :actions (list "q" (lambda (_) (bury-buffer))
                        "?" (lambda (_) (call-interactively #'listen-menu))
                        "g" (lambda (_) (call-interactively #'listen-queue-list))
                        "n" (lambda (_) (forward-line 1))
                        "p" (lambda (_) (forward-line -1))
                        "R" (lambda (queue)
                              (listen-queue-rename
                               (read-string (format "Rename queue %S: " (listen-queue-name queue)))
                               queue)
                              (call-interactively #'vtable-revert-command))
                        "C-k" #'listen-queue-discard
                        "RET" #'listen-queue
                        "SPC" (lambda (_) (call-interactively #'listen-pause))
                        "l" (lambda (queue)
                              (listen-library-from-queue :queue queue))
                        ;; "!" (lambda (_) (call-interactively #'listen-queue-shell-command))
                        )))
      (goto-char (point-min))
      (hl-line-mode 1)
      (listen-queue--highlight-current))
    ;; NOTE: We pop to the buffer outside of `with-current-buffer' so
    ;; `listen-queue--bookmark-handler' works correctly.
    (pop-to-buffer buffer)))

;;;; Vtable

(require 'vtable)

(cl-defmacro listen-with-vtable-at (position &rest body)
  "FIXME: Docstring."
  (declare (indent defun))
  (let ((positionᵥ (gensym)))
    `(let ((,positionᵥ ,position))
       (save-excursion
         (goto-char ,positionᵥ)
         (cl-letf* (((symbol-function 'frame-terminal)
                     (lambda (&optional _)
                       listen-vtable-frame-terminal))
                    ((symbol-function 'window-width)
                     (lambda (&optional _ _)
                       listen-vtable-window-width))
                    (table (vtable-current-table))
                    ((symbol-function 'vtable-current-table)
                     (lambda ()
                       table))
                    ((symbol-function 'vtable--recompute-numerical)
                     #'listen-queue--vtable--recompute-numerical))
           ,@body)))))

(defvar-local listen-vtable-frame-terminal nil)
(defvar-local listen-vtable-window-width nil)

(defun listen-make-vtable (&rest args)
  (apply #'make-vtable args)
  (setq-local listen-vtable-frame-terminal (frame-terminal)
              listen-vtable-window-width (window-width)))

(defalias 'listen-queue--vtable-update-object
  (if (version<= emacs-version "30")
      ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=69664>.
      (lambda (table object old-object)
        "Replace OLD-OBJECT in TABLE with OBJECT."
        (let* ((objects (vtable-objects table))
               (inhibit-read-only t))
          ;; First replace the object in the object storage.
          (if (eq old-object (car objects))
              ;; It's at the head, so replace it there.
              (setf (vtable-objects table)
                    (cons object (cdr objects)))
            ;; Otherwise splice into the list.
            (while (and (cdr objects)
                        (not (eq (cadr objects) old-object)))
              (setq objects (cdr objects)))
            (unless objects
              (error "Can't find the old object"))
            (setcar (cdr objects) object))
          ;; Then update the cache...
          ;; NOTE: This only works if the vtable's buffer is visible and its window has the same
          ;; width, or if the selected window happens to have the same width as the one last used to
          ;; display the vtable; otherwise, the cache will always be empty, so the old-object will
          ;; never be found, and an error will be signaled.
          (if-let ((line-number (seq-position (car (vtable--cache table)) old-object
                                              (lambda (a b)
                                                (equal (car a) b))))
                   (line (elt (car (vtable--cache table)) line-number)))
              (progn
                (setcar line object)
                (setcdr line (vtable--compute-cached-line table object))
                ;; ... and redisplay the line in question.
                (save-excursion
                  (vtable-goto-object old-object)
                  (let ((keymap (get-text-property (point) 'keymap))
                        (start (point)))
                    (delete-line)
                    (vtable--insert-line table line line-number
                                         (nth 1 (vtable--cache table))
                                         (vtable--spacer table))
                    (add-text-properties start (point) (list 'keymap keymap
                                                             'vtable table))))
                ;; We may have inserted a non-numerical value into a previously
                ;; all-numerical table, so recompute.
                (vtable--recompute-numerical table (cdr line)))
            (error "Can't find cached object in vtable"))))
    #'vtable-update-object))

;;;;; Functions using these vtable workarounds

(defun listen-queue-update-track (track queue)
  "Update TRACK in QUEUE.
Reverts TRACK's metadata from the file and updates it in QUEUE,
including QUEUE's buffer, if any."
  ;; TODO: Use where appropriate.
  (listen-queue-revert-track track)
  (listen-queue-with-buffer queue
    (listen-save-position
      (listen-with-vtable-at (point-min)
        (listen-queue--vtable-update-object table track track)))))

;;;###autoload
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
  (declare-function listen-mode "listen")
  (declare-function listen-play "listen")
  (let ((player (listen-current-player)))
    (listen-play player (listen-track-filename track))
    ;; Remember queue position of track so if it gets removed, we can still go to the next track.
    (setf (map-elt (listen-queue-etc queue) :track-number)
          (seq-position (listen-queue-tracks queue) track))
    (let ((previous-track (listen-queue-current queue)))
      (setf (listen-queue-current queue) track
            (map-elt (listen-player-etc player) :queue) queue)
      (listen-queue-with-buffer queue
        (listen-save-position
          (listen-with-vtable-at (point-min)
            (when previous-track
              (listen-queue--vtable-update-object table previous-track previous-track))
            (listen-queue--vtable-update-object table track track)))
        (listen-queue--highlight-current))))
  (unless listen-mode
    (listen-mode))
  queue)

(defalias 'listen-queue--vtable--recompute-numerical
  ;; TODO: Remove this when requiring Emacs 30+.
  ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=69927>.
  (if (version< emacs-version "30.1")
      (lambda (table line)
        "Recompute numericalness of columns if necessary."
        (let ((columns (vtable-columns table))
              (recompute nil))
          (seq-do-indexed
           (lambda (elem index)
             (when (and (vtable-column--numerical (elt columns index))
                        (not (numberp (car elem))))
               (setq recompute t)))
           line)
          (when recompute
            (vtable--compute-columns table))))
    ;; HACK: This is unique, one the likes of which I've never quite seen before.  This is to work
    ;; around errors like "(cyclic-function-indirection vtable--recompute-numerical)" and "Symbol’s
    ;; chain of function indirections contains a loop: vtable--recompute-numerical".  Because we
    ;; also, in the macro `listen-with-vtable-at', dynamically rebind the function
    ;; `vtable--recompute-numerical' with `cl-letf*', which normally creates a loop, we use
    ;; `cl-letf' here also, to save a reference to the original function definition, which we make
    ;; our own alias to.  Then when the expansion of `cl-letf*' in `listen-with-vtable-at' rebinds
    ;; the function slot of `vtable--recompute-numerical', it binds it to the original function,
    ;; rather than to the symbol (which would cause the cyclic indirection/loop).

    ;; Now, you may think this is ugly or ridiculous, but it has a legitimate purpose: to provide a
    ;; fix for users of older Emacs versions, while also being compatible with the Emacs version
    ;; that has the fix included.  And how many other languages and platforms would even allow this?
    ;; (Remember that `cl-letf' rebinds the symbol's function slot, so that while the macro's
    ;; expansion is on the stack, anything else--in the whole system--that calls the rebound
    ;; function calls our replacement for it--not just where we, ourselves, directly reference it.)
    (cl-letf ((orig-fn (symbol-function 'vtable--recompute-numerical)))
      orig-fn)))

;;;; Footer

(provide 'listen-queue)

;;; listen-queue.el ends here
