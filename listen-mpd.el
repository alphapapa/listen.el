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
(require 'map)
(require 'mpc)

(require 'listen-lib)

(defvar listen-directory)
(defvar crm-separator)

(declare-function listen-library "listen-library")
;;;###autoload
(cl-defun listen-library-from-mpd (tracks &key name)
  "Show library view of TRACKS selected from MPD library.
With prefix, select individual tracks with
`completing-read-multiple'; otherwise show all results.  NAME is
applied to the buffer."
  ;; FIXME: This isn't the ideal way to split functionality between the interactive form and the
  ;; function body.
  (interactive
   (if current-prefix-arg
       (list (listen-mpd-completing-read))
     (pcase-let ((`(,tag ,query) (listen-mpd-read-query :select-tag-p t)))
       (list `(lambda ()
                (listen-mpd-tracks-matching ,query :tag ',tag))
             :name (when query
                     (format "(MPD: %s%s)"
                             (if tag (format "%s:" tag) "")
                             query))))))
  (listen-library tracks :name name))

(declare-function listen-queue-add-files "listen-queue")
(declare-function listen-queue-complete "listen-queue-complete")
;;;###autoload
(cl-defun listen-queue-add-from-mpd (tracks queue)
  "Add TRACKS (selected from MPD library) to QUEUE."
  (interactive
   (let ((tracks (if current-prefix-arg
                     (listen-mpd-completing-read)
                   (pcase-let* ((`(,tag ,query) (listen-mpd-read-query :select-tag-p t)))
                     (listen-mpd-tracks-matching query :tag tag))))
         (queue (listen-queue-complete :prompt "Add to queue" :allow-new-p t)))
     (list tracks queue)))
  (require 'listen-mpd)
  (declare-function listen-queue-add-tracks "listen-queue")
  (listen-queue-add-tracks tracks queue))

(cl-defun listen-mpd-read-query (&key (tag 'file) select-tag-p)
  "Return MPD (TAG QUERY) read from the user.
If SELECT-TAG-P, read TAG with completion."
  (when select-tag-p
    (let ((tags '( artist album title track name genre date composer performer comment
                   disc file any)))
      (setf tag (completing-read "Search by tag: " tags nil t))))
  (cl-values tag (read-string (pcase-exhaustive tag
                                ('file "MPC Search (track): ")
                                (_ (format "MPC Search (%s): " tag))))))

;;;###autoload
(cl-defun listen-mpd-tracks-matching (query &key (tag "file") select-tag-p)
  "Return tracks matching QUERY on TAG.
If SELECT-TAG-P, prompt for TAG with completion.  If QUERY is
nil, read it."
  (when select-tag-p
    (let ((tags '( artist album title track name genre date composer performer comment
                   disc file any)))
      (setf tag (completing-read "Search by tag: " tags nil t))))
  (unless query
    (setf query (read-string (pcase-exhaustive tag
                               ('file "MPC Search (track): ")
                               (_ (format "MPC Search (%s): " tag))))))
  (let* ((command (cons "search" (list tag query)))
         (results (mpc-proc-buf-to-alists (mpc-proc-cmd command))))
    (when results
      (mapcar #'listen-mpd-track-for results))))

(defun listen-mpd-track-for (alist)
  "Return `listen-track' for MPD track ALIST."
  (pcase-let (((map file Artist Title Album Genre Date duration ('Track number)) alist))
    (make-listen-track
     :filename (expand-file-name file (or mpc-mpd-music-directory listen-directory))
     :artist Artist :title Title :album Album :genre Genre :date Date :number number
     :duration (when duration
                 (string-to-number duration))
     :metadata (map-apply (lambda (key value)
                            ;; TODO: Consider using symbols for keys everywhere to reduce consing.
                            (cons (downcase (symbol-name key))
                                  value))
                          alist))))

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
