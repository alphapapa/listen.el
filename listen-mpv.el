;;; listen-mpv.el --- MPV support for Emacs Music Player                    -*- lexical-binding: t; -*-

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

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'listen-lib)

;;;; Types

(cl-defstruct
    (listen-player-mpv
     (:include listen-player
               (command "mpv")
               (args '("--no-msg-color" "--idle"))
               (max-volume 100)
               (etc '((:request-id . 0))))))

;;;; Functions

(cl-defmethod listen--info ((player listen-player-mpv))
  "Return metadata from MPV PLAYER, or nil if a track is not playing."
  ;; If the metadata property isn't available, ignore the error.
  (when-let ((metadata (ignore-errors (listen-mpv--get-property player "metadata"))))
    (map-apply (lambda (key value)
                 ;; TODO: Consider using symbols as keys (VLC returns strings, MPV's decodes as
                 ;; symbols).
                 (cons (downcase (symbol-name key)) value))
               metadata)))

(cl-defmethod listen--filename ((player listen-player-mpv))
  "Return filename of PLAYER's current track."
  (let ((status (listen-mpv--get-property player "path" )))
    (when (string-match (rx bol "( new input: file://" (group (1+ nonl)) " )" ) status)
      (match-string 1 status))))

(cl-defmethod listen--title ((player listen-player-mpv))
  (listen-mpv--get-property player "media-title" ))

(cl-defmethod listen--ensure ((player listen-player-mpv))
  "Ensure PLAYER is ready."
  (pcase-let* (((cl-struct listen-player command args process) player)
               (socket (make-temp-name (expand-file-name "listen-mpv-socket-" temporary-file-directory)))
               (args (append args (list (format "--input-ipc-server=%s" socket)))))
    (unless (process-live-p process)
      (let ((process-buffer (generate-new-buffer " *listen-player-mpv*"))
            (socket-buffer (generate-new-buffer " *listen-player-mpv-socket*")))
        (buffer-disable-undo process-buffer)
        (buffer-disable-undo socket-buffer)
        (setf (listen-player-process player)
              (apply #'start-process "listen-player-mpv" process-buffer
                     command args))
        ;; FIXME: Test in a short loop rather than just sleeping for a second.
        (sleep-for 1)
        (setf (map-elt (listen-player-etc player) :network-process)
              (make-network-process :name "listen-player-mpv-socket" :family 'local
                                    :remote socket :noquery t
                                    :buffer socket-buffer)))
      (set-process-query-on-exit-flag (listen-player-process player) nil))))

(cl-defmethod listen--play ((player listen-player-mpv) file)
  "Play FILE with PLAYER.
Stops playing, clears playlist, adds FILE, and plays it."
  (listen--send player "loadfile" (expand-file-name file)))

;; (cl-defmethod listen--stop ((player listen-player-mpv))
;;   "Stop playing with PLAYER."
;;   (listen--send player "stop"))

(cl-defmethod listen--status ((player listen-player-mpv))
  (if (and (listen--playing-p player)
           (not (listen-mpv--get-property listen-player "pause")))
      "playing"
    ;; TODO: Consider using "eof-reached" proeprty.
    (if (listen-mpv--get-property listen-player "pause")
        "paused"
      "stopped")))

(cl-defmethod listen--pause ((player listen-player-mpv))
  "Pause playing with PLAYER."
  (if (listen-mpv--get-property player "pause")
      (listen-mpv--set-property player "pause" "no")
    (listen-mpv--set-property player "pause" "yes")))

(cl-defmethod listen--playing-p ((player listen-player-mpv))
  "Return non-nil if PLAYER is playing."
  (not (listen-mpv--get-property player "idle-active")))

(cl-defmethod listen--elapsed ((player listen-player-mpv))
  "Return seconds elapsed for PLAYER's track."
  (listen-mpv--get-property player "time-pos"))

(cl-defmethod listen--length ((player listen-player-mpv))
  "Return length of PLAYER's track in seconds."
  (listen-mpv--get-property player "duration"))

(require 'json)

(cl-defmethod listen--send ((player listen-player-mpv) command &rest args)
  "Send COMMAND to PLAYER and return output."
  (listen--ensure player)
  (pcase-let* (((cl-struct listen-player (etc (map :network-process))) player)
               (request-id (cl-incf (map-elt (listen-player-etc player) :request-id))))
    (with-current-buffer (process-buffer network-process)
      (let ((json (json-encode `(("command" ,command ,@args)
                                 ("request_id" . ,request-id)))))
        ;; (message "SENDING: %S" json)
        (process-send-string network-process json)
        (process-send-string network-process "\n")
        (goto-char (point-max))
        (with-local-quit
          (accept-process-output network-process 2))
        (save-excursion
          (goto-char (point-min))
          (let ((json-false nil))
            (cl-loop
             ;; do (message "BUFFER-CONTENTS:%S  POS:%s  BUFFER-SIZE:%s  EOBP:%s"
             ;;             (buffer-string) (point) (buffer-size) (eobp))
             until (or (eobp) (looking-at-p (rx (0+ space) eos)))
             for start-pos = (point)
             for result = (condition-case-unless-debug err
                              (json-read)
                            (error
                             (message "listen--send: JSON-READ signaled error: %S  BUFFER-CONTENTS:%S  POS:%s  BUFFER-SIZE:%s  EOBP:%s"
                                      err (buffer-string) (point) (buffer-size) (eobp))))
             while result
             for value = (pcase (map-elt result 'request_id)
                           ((pred (equal request-id))
                            ;; Event is the one we're looking for: delete the event from the
                            ;; buffer and return it.
                            (unless listen-debug-p
                              (delete-region start-pos (point)))
                            result)
                           ('nil
                            ;; Event has no request ID: delete it from the buffer.
                            (unless listen-debug-p
                              (delete-region start-pos (point)))
                            nil)
                           (_
                            ;; Event is for a different request: ignore it (this probably
                            ;; won't happen in practice, since we process commands
                            ;; synchronously, but it's good to be careful).
                            nil))
             when value
             return value)))))))

(cl-defmethod listen--seek ((player listen-player-mpv) seconds)
  "Seek PLAYER to SECONDS."
  (listen--send player "seek" seconds "absolute"))

(cl-defmethod listen--volume ((player listen-player-mpv) &optional volume)
  "Return or set PLAYER's VOLUME.
VOLUME is an integer percentage."
  (pcase-let (((cl-struct listen-player max-volume) player))
    (if volume
        (progn
          (unless (<= 0 volume max-volume)
            (error "VOLUME must be 0-%s" max-volume))
          (listen-mpv--set-property player "volume" volume))
      (listen-mpv--get-property player "volume"))))

(cl-defmethod listen-mpv--get-property ((player listen-player-mpv) property)
  (pcase-let (((map error data) (listen--send player "get_property" property)))
    (pcase error
      ("success" data)
      (_ (condition-case-unless-debug _
             ;; Between tracks, getting a property may fail, which should generally be ignored.
             (error "listen-mpv--get-property: Getting property %S failed: %S" property error)
           (error nil))))))

(cl-defmethod listen-mpv--set-property ((player listen-player-mpv) property &rest args)
  (pcase-let (((map error data) (apply #'listen--send player "set_property" property args)))
    (pcase error
      ("success" data)
      (_ (error "listen-mpv--set-property: Setting property %S failed: %S" property error)))))

(provide 'listen-mpv)

;;; listen-mpv.el ends here
