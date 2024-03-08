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

;; 

;;; Code:

(require 'cl-lib)

;;;; Macros

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
  process command args etc)

(cl-defstruct listen-queue
  name tracks current etc)

(cl-defstruct listen-track
  ;; FIXME: Store rating in the slot I already made for it.
  ;; FIXME: Put metadata in its slot rather than etc.
  ;; NOTE: All of the metadata values are stored as strings, except for duration.
  filename artist title album number genre (duration 0) date rating etc metadata)

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

;;;; Functions

;; FIXME: Declare this differently or something.
(declare-function make-listen-player-vlc "listen-vlc")
(defun listen-current-player ()
  "Return variable `listen-player' or a newly set one if nil."
  (or listen-player
      (setf listen-player (make-listen-player-vlc))))

(defun listen-format-seconds (seconds)
  "Return SECONDS formatted as an hour:minute:second-style duration."
  (format-seconds "%h:%z%.2m:%.2s" seconds))

;;;; Methods

(cl-defgeneric listen--elapsed (player)
  "Return elapsed seconds of PLAYER's current track.")

(cl-defgeneric listen--length (player)
  "Return duration in seconds of PLAYER's current track.")

(cl-defmethod listen--running-p ((player listen-player))
  "Return non-nil if PLAYER is running."
  (process-live-p (listen-player-process player)))

;;;; Footer

(provide 'listen-lib)

;;; listen-lib.el ends here
