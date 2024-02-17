;;; listen-lib.el --- Library code for listen        -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;;;; Types

(cl-defstruct listen-player
  process command args)

(cl-defstruct listen-queue
  name tracks current etc)

(cl-defstruct listen-track
  filename artist title album number genre length date rating etc)

;;;; Variables

(defvar listen-debug-p nil
  "When non-nil, don't erase process buffer after sending commands.")

;;;; Footer

(provide 'listen-lib)

;;; listen-lib.el ends here
