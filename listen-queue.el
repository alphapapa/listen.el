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

(defcustom listen-queue-max-probe-processes 16
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
;;;###autoload
(defun listen-queue (queue)
  "Show listen QUEUE."
  (interactive (list (listen-queue-complete)))
  (let* ((buffer-name (format "*Listen Queue: %s*" (listen-queue-name queue)))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (with-current-buffer (setf buffer (get-buffer-create buffer-name))
        (let ((inhibit-read-only t))
          (setf listen-queue queue)
          (read-only-mode)
          (erase-buffer)
          (toggle-truncate-lines 1)
          (setq-local bookmark-make-record-function #'listen-queue--bookmark-make-record)
          (when (listen-queue-tracks listen-queue)
            (make-vtable
             :columns
             (list (list :name "▶" :primary 'descend
                         :getter (lambda (track _table)
                                   (if (eq track (listen-queue-current queue))
                                       ;; FIXME: If track metadata changes during playback and the
                                       ;; user refreshes the queue from disk, the currently playing
                                       ;; track won't match anymore.  (The obvious solution is to
                                       ;; compare filenames, but that would seem wasteful for a
                                       ;; large queue, so let's defer that for now.)
                                       "▶" " ")))
                   (list :name "#" :primary 'descend
                         :getter (lambda (track _table)
                                   (cl-position track (listen-queue-tracks queue))))
                   (list :name "Duration"
                         :getter (lambda (track _table)
                                   (when-let ((duration (listen-track-duration track)))
                                     (listen-format-seconds duration))))
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
                   (list :name "#"
                         :getter (lambda (track _table)
                                   (or (listen-track-number track) "")))
                   (list :name "Date"
                         :getter (lambda (track _table)
                                   (or (listen-track-date track) "")))
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
                            "!" (lambda (_) (call-interactively #'listen-queue-shell-command))))
            (vtable-end-of-table)
            (insert (format "Duration: %s"
                            (listen-format-seconds (cl-reduce #'+ (listen-queue-tracks queue)
                                                              :key #'listen-track-duration)))))
          (goto-char (point-min))
          (listen-queue--highlight-current)
          (hl-line-mode 1))))
    (pop-to-buffer buffer)))

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
  (when-let ((buffer (listen-queue-buffer queue)))
    (with-current-buffer buffer
      ;; `save-excursion' doesn't work because of the table's being reverted.
      (let ((pos (point)))
        (goto-char (point-min))
        (when (vtable-current-table)
          (vtable-revert-command))
        (goto-char pos)
        (goto-char (pos-bol)))
      (listen-queue--highlight-current)
      (listen-queue-goto-current))))

(declare-function listen-mode "listen")
(declare-function listen-play "listen")
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
  (let ((player (listen--player)))
    (listen-play player (listen-track-filename track))
    (setf (listen-queue-current queue) track
          (map-elt (listen-player-etc player) :queue) queue)
    (listen-queue--update-buffer queue))
  (unless listen-mode
    (listen-mode))
  queue)

(defun listen-queue-goto-current ()
  "Jump to current track."
  (interactive)
  (when-let ((current-track (listen-queue-current listen-queue)))
    (vtable-goto-object current-track)))

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
                (let* ((player (listen--player))
                       (default-queue-name (or (when listen-queue
                                                 ;; In a listen buffer: offer its queue as default.
                                                 (listen-queue-name listen-queue))
                                               (when (listen--playing-p player)
                                                 (listen-queue-name (map-elt (listen-player-etc player) :queue)))))
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

;;;###autoload
(cl-defun listen-queue-add-files (files queue)
  "Add FILES to QUEUE."
  (interactive
   (let ((queue (listen-queue-complete :allow-new-p t))
         (path (expand-file-name (read-file-name "Enqueue file/directory: " listen-directory nil t))))
     (list (if (file-directory-p path)
               (directory-files-recursively path ".")
             (list path))
           queue)))
  (cl-callf append (listen-queue-tracks queue) (listen-queue-tracks-for files))
  (listen-queue queue)
  (listen-queue-play queue)
  queue)

(declare-function listen-mpd-completing-read "listen-mpd")
(cl-defun listen-queue-add-from-mpd (filenames queue)
  "Add FILENAMES (selected from MPD library) to QUEUE."
  (interactive
   (list (listen-mpd-completing-read :select-tag-p t)
         (listen-queue-complete :allow-new-p t)))
  (require 'listen-mpd)
  (listen-queue-add-files filenames queue))

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
          (tracks (or (if-let ((buffer (listen-queue-buffer queue)))
                          (with-current-buffer buffer
                            (when (region-active-p)
                              (listen-queue-selected))))
                      (listen-queue-tracks queue))))
     (list :tracks tracks)))
  (let ((tracks (or tracks (listen-queue-tracks queue))))
    (listen-library (mapcar #'listen-track-filename tracks))))

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
     :genre (map-elt metadata "genre"))))

(defun listen-queue-tracks-for (filenames)
  "Return tracks for FILENAMES.
When `listen-queue-ffprobe-p' is non-nil, adds durations read
with \"ffprobe\"."
  (with-demoted-errors "listen-queue-tracks-for: %S"
    (let ((tracks (remq nil (mapcar #'listen-queue-track filenames))))
      (when listen-queue-ffprobe-p
        (listen-queue--add-track-durations tracks))
      tracks)))

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
                          :key #'listen-track-filename)
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
      (seq-elt (listen-queue-tracks queue)
               (1+ (seq-position (listen-queue-tracks queue)
                                 (listen-queue-current queue)
                                 (lambda (a b)
                                   (equal (listen-track-filename a)
                                          (listen-track-filename b))))))))

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

(cl-defun listen-queue-revert (queue &key refreshp)
  "Revert QUEUE's buffer.
When REFRESHP (interactively, with prefix), refresh tracks from
disk."
  (interactive (list listen-queue :refreshp current-prefix-arg))
  (when refreshp
    (listen-queue-refresh queue))
  (listen-queue--update-buffer queue))

(defun listen-queue-refresh (queue)
  "Refresh QUEUE's tracks from disk."
  (setf (listen-queue-tracks queue)
        (listen-queue-tracks-for (mapcar #'listen-track-filename (listen-queue-tracks queue)))))

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
                                :name "listen:ffprobe" :noquery t :type 'pipe :buffer (current-buffer)
                                :sentinel sentinel :command (if listen-queue-nice-p
                                                                (cons "nice" command)
                                                              command))))
                 process))))
         (probe-more ()
           (while (and tracks (length< processes max-processes))
             (let ((track (pop tracks)))
               (push (probe-duration track) processes)))))
      (with-timeout ((* 0.1 (length tracks)) (error "Probing for track duration timed out"))
        (while (or tracks processes)
          (probe-more)
          (while (accept-process-output nil 0.01))
          (sleep-for 0.01))))))

;;;; Footer

(provide 'listen-queue)

;;; listen-queue.el ends here
