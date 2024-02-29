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

;;;; Types

(cl-defstruct listen-player
  ;; TODO: Add queue slot.
  process command args etc)

(cl-defstruct listen-queue
  name tracks current etc)

(cl-defstruct listen-track
  filename artist title album number genre length date rating etc)

(cl-defmethod cl-print-object ((track listen-track) stream)
  (prin1 (listen-track-filename track) stream))

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

(defface listen-album '((t :inherit font-lock-doc-face))
  "Track album.")

(defface listen-filename '((t :inherit fixed-pitch))
  "Track filename.")

(defface listen-genre '((t :inherit font-lock-type-face))
  "Track genre.")

;;;; Functions

;; FIXME: Declare this differently or something.
(declare-function make-listen-player-vlc "listen-vlc")
(defun listen--player ()
  "Return variable `listen-player' or a newly set one if nil."
  (or listen-player
      (setf listen-player (make-listen-player-vlc))))

;;;; Methods

(cl-defmethod listen--running-p ((player listen-player))
  "Return non-nil if PLAYER is running."
  (process-live-p (listen-player-process player)))

;;;; Footer

(provide 'listen-lib)

;;; listen-lib.el ends here
