;;; listen-mpd.el --- MPD source for Listen          -*- lexical-binding: t; -*-

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

;; This library provides a function to select filenames from an MPD
;; server's library.  It requires that the user has set the option
;; `mpc-mpd-music-directory', or that `listen-directory' be the MPD
;; server's music directory.

;;; Code:

(require 'cl-lib)
(require 'mpc)

(defvar listen-directory)
(defvar crm-separator)

;;;###autoload
(cl-defun listen-mpd-completing-read (&key (tag 'file) select-tag-p)
  "Return files selected from MPD library.
Searches by TAG; or if SELECT-TAG-P, tag is selected with
completion."
  (cl-assert (file-directory-p (or mpc-mpd-music-directory listen-directory)))
  (cl-labels ((search-any (queries)
                (mpc-proc-buf-to-alists
                 (mpc-proc-cmd (cl-loop for query in queries
                                        append (list "any" query)
                                        into list
                                        finally return (cons "search" list)))))
              (column-size (column completions)
                (cl-loop for completion in completions
                         for alist = (get-text-property 0 :mpc-alist completion)
                         maximizing (string-width (or (alist-get column alist) ""))))
              (align-to (pos string)
                (when string
                  (concat (propertize " "
                                      'display `(space :align-to ,pos))
                          string)))
              (affix (completions)
                (when completions
                  (let* ((artist-width (column-size 'Artist completions))
                         ;; (album-width (column-size 'Album completions))
                         (title-width (column-size 'Title completions))
                         (title-start (+ artist-width 2))
                         (album-start (+ title-start title-width 2)))
                    (cl-loop for completion in completions
                             for alist = (get-text-property 0 :mpc-alist completion)
                             for artist = (alist-get 'Artist alist)
                             for album = (alist-get 'Album alist)
                             for title = (alist-get 'Title alist)
                             for date = (alist-get 'Date alist)
                             do (progn
                                  (add-face-text-property 0 (length prefix)
                                                          'font-lock-doc-face t prefix)
                                  (add-face-text-property 0 (length album)
                                                          '(:slant italic) nil album)
                                  (add-face-text-property 0 (length title)
                                                          '(:underline t) nil title)
                                  (when album
                                    (setf album (align-to album-start album)))
                                  (when title
                                    (setf title (align-to title-start title)))
                                  (when date
                                    (setf date (format " (%s)" date))))
                             for prefix = (concat artist "  " title "" album "" date)
                             collect (list (align-to 'center completion)
                                           prefix
                                           "")))))
              (collection (str _pred flag)
                (pcase flag
                  ('metadata (pcase tag
                               ('any
                                (list 'metadata
                                      (cons 'affixation-function #'affix)
                                      ;; (cons 'annotation-function #'annotate)
                                      ))))
                  (`t (unless (string-empty-p str)
                        (let ((tag (pcase tag
                                     ('any 'file)
                                     (_ tag))))
                          (delete-dups
                           (delq nil
                                 (mapcar (lambda (row)
                                           (when-let ((value (alist-get tag row)))
                                             (propertize value
                                                         :mpc-alist row)))
                                         (search-any (split-string str))))))))))
              (try (string _table _pred point &optional _metadata)
                (cons string point))
              (all (string table pred _point)
                (all-completions string table pred)))
    (when select-tag-p
      (let ((tags '( Artist Album Title Track Name Genre Date Composer Performer Comment
                     Disc file any)))
        (setf tag (intern (completing-read "Search by tag: " tags nil t)))))
    (let* ((completion-styles '(listen-mpc-completing-read))
           (completion-styles-alist (list (list 'listen-mpc-completing-read #'try #'all
                                                "Listen-MPC completing read")))
           (prompt (pcase-exhaustive tag
                     ('file "MPC Search (track): ")
                     (_ (format "MPC Search (%s): " tag))))
           (result (let ((crm-separator ";"))
                     (ensure-list (completing-read-multiple prompt #'collection nil))))
           (result (pcase tag
                     ('any result)
                     (_ (flatten-list
                         (mapcar (lambda (result)
                                   (mapcar (lambda (row)
                                             (alist-get 'file row))
                                           (mpc-proc-buf-to-alists
                                            (mpc-proc-cmd (list "find" (symbol-name tag) result)))))
                                 result))))))
      (mapcar (lambda (filename)
                (expand-file-name filename (or mpc-mpd-music-directory listen-directory)))
              result))))

(provide 'listen-mpd)
;;; listen-mpd.el ends here
