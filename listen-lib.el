;;; listen-lib.el --- Library code for listen        -*- lexical-binding: t; -*-

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

;; Library functions for Listen.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'pcase)
(require 'warnings)

;;;; Macros

(cl-defmacro listen-debug (&rest args)
  "Display a debug warning showing the runtime value of ARGS.
The warning automatically includes the name of the containing
function, and it is only displayed if `warning-minimum-log-level'
is `:debug' at expansion time (otherwise the macro expands to a
call to `ignore' with ARGS and is eliminated by the
byte-compiler).  When debugging, the form also returns nil so,
e.g. it may be used in a conditional in place of nil.

Each of ARGS may be a string, which is displayed as-is, or a
symbol, the value of which is displayed prefixed by its name, or
a Lisp form, which is displayed prefixed by its first symbol.

Before the actual ARGS arguments, you can write keyword
arguments, i.e. alternating keywords and values.  The following
keywords are supported:

  :buffer BUFFER   Name of buffer to pass to `display-warning'.
  :level  LEVEL    Level passed to `display-warning', which see.
                   Default is :debug."
  ;; TODO: Can we use a compiler macro to handle this more elegantly?
  (pcase-let* ((fn-name (when byte-compile-current-buffer
                          (with-current-buffer byte-compile-current-buffer
                            ;; This is a hack, but a nifty one.
                            (save-excursion
                              (beginning-of-defun)
                              (cl-second (read (current-buffer)))))))
               (plist-args (cl-loop while (keywordp (car args))
                                    collect (pop args)
                                    collect (pop args)))
               ((map (:buffer buffer) (:level level)) plist-args)
               (level (or level :debug))
               (string (cl-loop for arg in args
                                concat (pcase arg
                                         ((pred stringp) "%S ")
                                         ((pred symbolp)
                                          (concat (upcase (symbol-name arg)) ":%S "))
                                         ((pred listp)
                                          (concat "(" (upcase (symbol-name (car arg)))
                                                  (pcase (length arg)
                                                    (1 ")")
                                                    (_ "...)"))
                                                  ":%S "))))))
    (if (eq :debug warning-minimum-log-level)
        `(let ((fn-name ,(if fn-name
                             `',fn-name
                           ;; In an interpreted function: use `backtrace-frame' to get the
                           ;; function name (we have to use a little hackery to figure out
                           ;; how far up the frame to look, but this seems to work).
                           `(cl-loop for frame in (backtrace-frames)
                                     for fn = (cl-second frame)
                                     when (not (or (subrp fn)
                                                   (special-form-p fn)
                                                   (eq 'backtrace-frames fn)))
                                     return (make-symbol (format "%s [interpreted]" fn))))))
           (display-warning fn-name (format ,string ,@args) ,level ,buffer)
           nil)
      `(ignore ,@args))))

(defmacro listen-once-per (value-form &rest body)
  "Evaluate BODY at most once while VALUE-FORM has the same value."
  (declare (indent defun))
  (let ((value-defvar (gensym "listen-once-per"))
        (value-var (gensym "listen-once-per")))
    `(progn
       (defvar ,value-defvar nil
         "Defined by macro `listen-once-per', which see.")
       (let ((,value-var ,value-form))
         (unless (equal ,value-defvar ,value-var)
           (setf ,value-defvar ,value-var)
           ,@body)))))

;;;; Types

(cl-defstruct listen-player
  ;; TODO: Add queue slot.
  process command args
  (status
   nil :documentation "Symbol representing player's playback status.
For example, `playing', `paused', `stopped', or nil if unknown.")
  (etc nil :documentation "Alist used to store other information about the player.")
  (path nil :documentation "Filename path or URL to currently playing track, if any.")
  (metadata nil :documentation "Metadata alist.")
  (volume nil :documentation "Volume in percent.")
  (max-volume
   100 :documentation "Maximum volume in percent (may be greater than 100 for some players).")
  (playback-started-at
   nil :documentation "Time at which playback started (used to compute elapsed/remaining).")
  (playback-started-from
   nil :documentation "Track position at which playback last started/unpaused, in seconds.
Used to compute elapsed/remaining.")
  (duration
   nil :documentation "Duration of current track, in seconds (used to compute elapsed/remaining)."))

(cl-defstruct listen-queue
  name tracks current etc)

(cl-defstruct listen-track
  ;; FIXME: Store rating in the slot I already made for it.
  ;; FIXME: Put metadata in its slot rather than etc.
  ;; NOTE: All of the metadata values are stored as strings, except for duration.
  filename artist title album number genre (duration 0) date rating etc metadata)

(defun listen-track-metadata-get (key track)
  "Return value of KEY in TRACK's metadata.
If KEY appears in metadata multiple times (as multiple instances
of the key, or as a single instance with null-separated values),
return a list of values; otherwise return the sole value."
  ;; Don't use the null character directly, because it makes Git think it's a binary file.
  (cl-macrolet ((null-byte-string () (char-to-string #x0)))
    (let ((values (cl-loop for (k . v) in (listen-track-metadata track)
                           when (equal k key)
                           collect v)))
      (pcase (length values)
        (0 nil)
        (1 (let ((values (split-string (car values) (null-byte-string))))
             (pcase-exhaustive (length values)
               (1 (car values))
               (_ values))))
        (_ values)))))

;;;; Variables

(defvar listen-player nil)

(defvar listen-debug-p nil
  "When non-nil, don't erase process buffer after sending commands.")

;;;; Faces

(defgroup listen-faces nil
  "Faces used by `listen'."
  :group 'listen)

(defface listen-artist '((t :inherit font-lock-variable-name-face))
  "Track artist.")

(defface listen-title '((t :inherit font-lock-function-name-face))
  "Track title.")

(defface listen-album '((t :slant italic :inherit font-lock-doc-face))
  "Track album.")

(defface listen-filename '((t :inherit fixed-pitch))
  "Track filename.")

(defface listen-genre '((t :inherit font-lock-type-face))
  "Track genre.")

(defface listen-rating '((t :inherit font-lock-escape-face))
  "Track rating.")

(defgroup listen-lighter-faces nil
  "Faces used in the mode line lighter."
  :group 'listen-faces)

(defface listen-lighter-artist '((t :inherit listen-artist))
  "Track artist.")

(defface listen-lighter-title '((t :inherit listen-title))
  "Track title.")

(defface listen-lighter-album '((t :inherit listen-album))
  "Track album.")

(defface listen-lighter-filename '((t :inherit listen-filename))
  "Track filename.")

(defface listen-lighter-genre '((t :inherit listen-genre))
  "Track genre.")

(defface listen-lighter-rating '((t :inherit listen-rating))
  "Track rating.")

(defface listen-lighter-time '((t :inherit fixed-pitch))
  "Track time elapsed/remaining.")

(defface listen-lighter-extra '((t :inherit font-lock-comment-face))
  "See `listen-lighter-extra-functions'.")

;;;; Functions

(defun listen-current-player ()
  "Return variable `listen-player' or a newly set one if nil."
  (defvar listen-backend)
  (or listen-player
      (setf listen-player (funcall listen-backend))))

(cl-defun listen-current-track (&optional (player listen-player))
  "Return track playing on PLAYER, if any."
  ;; TODO: Use this where appropriate.
  (when-let ((player)
             (queue (alist-get :queue (listen-player-etc player))))
    (listen-queue-current queue)))

(defun listen-format-seconds (seconds)
  "Return SECONDS formatted as an hour:minute:second-style duration."
  (format-seconds "%h:%z%m:%.2s" seconds))

(define-hash-table-test
 'listen-track-equal
 #'equal
 (lambda (track)
   (sxhash-equal (expand-file-name (listen-track-filename track)))))

(cl-defun listen-delete-dups (list &optional (test 'listen-track-equal))
  "Return LIST having destructively removed duplicates.
Similar to `delete-dups', but TEST may be specified.
Unlike `delete-dups', this function always uses a hash table to find
duplicates; therefore TEST should be compatible with `make-hash-table',
which see."
  ;; Copies the body of `delete-dups', passing through TEST, and removing the length-based
  ;; non-hash-table case..
  (let ((hash (make-hash-table :test test))
        (tail list) retail)
    (puthash (car list) t hash)
    (while (setq retail (cdr tail))
      (let ((elt (car retail)))
        (if (gethash elt hash)
            (setcdr tail (cdr retail))
          (puthash elt t hash)
          (setq tail retail))))
    list))

;;;; Methods

(cl-defgeneric listen--elapsed (player)
  "Return elapsed seconds of PLAYER's current track.")

(cl-defgeneric listen--length (player)
  "Return duration in seconds of PLAYER's current track.")

(cl-defgeneric listen--playing-p (player)
  "Return non-nil if PLAYER is playing.")

(cl-defmethod listen--running-p ((player listen-player))
  "Return non-nil if PLAYER is running."
  (process-live-p (listen-player-process player)))

;;;; Footer

(provide 'listen-lib)

;;; listen-lib.el ends here
