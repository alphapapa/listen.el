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
(require 'json)
(require 'map)

(require 'listen-lib)

;;;; Customization

(defgroup listen-mpv nil
  "MPV-related options."
  :group 'listen)

(defcustom listen-mpv-volume 50
  "Initial volume for MPV instance."
  :type 'natnum)

;;;; Types

(cl-defstruct
    (listen-player-mpv
     (:include listen-player
               (command "mpv")
               (args '("--no-msg-color" "--idle" "--audio-display=no"))
               (max-volume 100)
               (etc '((:request-id . 0))))))

;;;; Functions

(cl-defmethod listen--info ((player listen-player-mpv))
  "Return metadata from MPV PLAYER, or nil if a track is not playing."
  (or (listen-player-metadata player)
      (listen--update-metadata player)))

(cl-defmethod listen--update-metadata ((player listen-player-mpv) &optional then)
  "Update PLAYER's metadata slot, then call THEN without arguments."
  (let ((callback (lambda (metadata)
                    (pcase metadata
                      ((and (or `nil :unknown) value)
                       ;; May happen between tracks.
                       (listen-debug "Metadata response was" value))
                      (_
                       (setf (listen-player-metadata player)
                             (map-apply (lambda (key value)
                                          (cons (intern (downcase (symbol-name key))) value))
                                        metadata))
                       (when then
                         (funcall then)))))))
    (if then
        (listen-mpv--get-property player "metadata" :then callback)
      (funcall callback (listen-mpv--get-property player "metadata")))))

(cl-defmethod listen--filename ((player listen-player-mpv))
  "Return filename of PLAYER's current track."
  (let ((new-status (listen-mpv--get-property player "path")))
    (when (string-match (rx bol "( new input: file://" (group (1+ nonl)) " )" ) new-status)
      (match-string 1 new-status))))

(cl-defmethod listen--title ((player listen-player-mpv))
  (map-elt (listen-player-metadata player) 'title))

(cl-defmethod listen--ensure ((player listen-player-mpv))
  "Ensure PLAYER is ready."
  (pcase-let* (((cl-struct listen-player command args process) player)
               (socket (make-temp-name (expand-file-name "listen-mpv-socket-" temporary-file-directory)))
               (args (append args (list (format "--input-ipc-server=%s" socket)
                                        "--msg-level=ipc=debug"
                                        (format "--volume=%s" listen-mpv-volume)
                                        "--terminal=no"))))
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
                                    :buffer socket-buffer
                                    :service nil)
              (process-filter (map-elt (listen-player-etc player) :network-process))
              (lambda (proc text)
                (listen--filter player proc text))
              (process-sentinel (map-elt (listen-player-etc player) :network-process))
              (lambda (proc msg)
                (display-warning 'listen-mpv
                                 (format-message "listen-process-sentinel: PROC:%S  MSG:%S"
                                                 proc msg)
                                 :debug "*listen-mpv*")
                (internal-default-process-sentinel proc msg))))
      (set-process-query-on-exit-flag (listen-player-process player) nil)
      ;; Observe relevant properties.
      (dolist (property '("volume" "mute" "pause" "playback-time" "duration" "path" "metadata"))
        (listen--send* player `("observe_property" ,property) :then #'ignore)))))

(cl-defmethod listen--filter ((player listen-player-mpv) proc text)
  (listen-debug :buffer "*listen-mpv*" (listen-player-process player) proc text)
  (cl-labels ((next-message ()
                (if-let ((msg (ignore-errors (let ((json-false nil))
                                               (json-read)))))
                    (progn
                      (listen-debug :buffer "*listen-mpv*" "Parsed" msg)
                      (delete-region (point-min) (point))
                      msg)
                  ;; Unparseable: return point so we can try again later.
                  (listen-debug :buffer "*listen-mpv*" "Unparseable")
                  (goto-char (point-min))
                  nil)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert text)
      (goto-char (point-min))
      (while-let ((msg (next-message)))
        (listen--act player msg)))))

(cl-defmethod listen--act ((player listen-player-mpv) msg)
  (listen-debug :buffer "*listen-mpv*" (listen-player-process player) msg)
  (pcase-let (((map event request_id _reason data _error name) msg))
    (pcase event
      ((or "start-file" "playback-restart")
       (listen--status-is player 'playing)
       (listen--update-metadata player)
       ;; TODO: Maybe move these to --status-is?
       (setf (listen-player-duration player) (listen-mpv--get-property player "duration"))
       (setf (listen-player-volume player) (listen-mpv--get-property player "volume")))
      ((or "end-file" "idle") (listen--status-is player 'stopped))
      ((or 'nil "data")
       (if-let ((callback (map-elt (map-elt (listen-player-etc player) :requests) request_id)))
           (prog1
               (funcall callback msg)
             (setf (map-elt (listen-player-etc player) :requests)
                   (map-delete (map-elt (listen-player-etc player) :requests) request_id)))
         (listen-debug :buffer "*listen-mpv*" "No callback for" msg)))
      ("property-change"
       ;; NOTE: Even though we explicitly observe these properties, if they change as a result of a
       ;; command that we send, MPV does not send messages for these properties changing (e.g. if we
       ;; tell it to pause, we don't get a pause property-change event).
       (pcase name
         ("duration" (setf (listen-player-duration player) data))
         ("metadata" (setf (listen-player-metadata player) data))
         ("path" (setf (listen-player-path player) data))
         ("pause"
          (listen--status-is
           player (pcase data
                    ('t 'paused)
                    ('nil 'playing)
                    (_ (listen-debug :buffer "*listen-mpv*" "Unrecognized pause" data)))))
         ;; ("playback-time" (setf (listen-player-position player) data
         ;;                        (listen-player-playback-started-from player) data))
         ("volume" (setf (listen-player-volume player) data))))
      (_ (listen-debug :buffer "*listen-mpv*" "Unrecognized event" event)))))

(cl-defmethod listen--status-is ((player listen-player-mpv) new-status)
  "Update PLAYER's status slot according to NEW-STATUS and return it.
When NEW-STATUS is `playing', updates started-at and started-from slots."
  (pcase-exhaustive new-status
    ('paused nil)
    ('playing
     (setf (listen-player-playback-started-at player) (current-time)
           (listen-player-playback-started-from player)
           (listen-mpv--get-property player "playback-time")))
    ('stopped (setf (listen-player-playback-started-at player) nil
                    (listen-player-playback-started-from player) nil)))
  (setf (listen-player-status player) new-status))

(cl-defmethod listen--play ((player listen-player-mpv) file)
  "Play FILE with PLAYER.
Stops playing, clears playlist, adds FILE, and plays it."
  (listen--send* player `("loadfile" ,(expand-file-name file)) :then #'ignore))

;; (cl-defmethod listen--stop ((player listen-player-mpv))
;;   "Stop playing with PLAYER."
;;   (listen--send player "stop"))

(cl-defmethod listen--status ((player listen-player-mpv))
  (listen-player-status player))

(cl-defmethod listen--pause ((player listen-player-mpv))
  "Pause playing with PLAYER."
  (let ((new-status (pcase (listen-player-status player)
                      ('playing "yes")
                      ('paused "no")
                      ('nil "no"))))
    (listen-mpv--set-property
     player "pause" new-status
     :then (lambda (msg)
             (pcase (map-elt msg 'error)
               ("success" (listen--status-is
                           player (pcase-exhaustive new-status ("yes" 'paused) ("no" 'playing))))
               (_ (display-warning 'listen--pause (format-message "Unexpected response: %S" msg)
                                   :warning "*listen-mpv*")))))))

(cl-defmethod listen--playing-p ((player listen-player-mpv))
  "Return non-nil if PLAYER is playing."
  (equal (listen-player-status player) 'playing))

(cl-defmethod listen--elapsed ((player listen-player-mpv))
  "Return seconds elapsed for PLAYER's track."
  (if (listen--playing-p player)
      (setf (map-elt (listen-player-etc player) :elapsed)
            (+ (time-to-seconds
                (time-subtract (current-time) (listen-player-playback-started-at player)))
               (listen-player-playback-started-from player)))
    (map-elt (listen-player-etc player) :elapsed)))

(cl-defmethod listen--length ((player listen-player-mpv))
  "Return length of PLAYER's track in seconds."
  (listen-player-duration player))

(cl-defmethod listen--send ((player listen-player-mpv) command &rest args)
  "Not implemented for MPV; use `listen--send*'.
For checkdoc: PLAYER, COMMAND, ARGS."
  (ignore player command args)
  (error "Method `listen--send' is not implemented for player `listen-player-mpv'; use `listen--send*'"))

(cl-defmethod listen--send* ((player listen-player-mpv) command-args &key then)
  "Send COMMAND-ARGS to PLAYER.
The first string in COMMAND-ARGS is the MPV command, and the remaining
ones are arguments to it.  If THEN is provided, it should be a function
which will be called asynchronously with the message alist returned by
MPV, and the request ID number is returned from this function;
otherwise, the MPV command is called synchronously and the message alist
is returned from this function."
  (listen--ensure player)
  (cl-macrolet
      ((wrap-callback (callback)
         `(lambda (msg)
            (unwind-protect
                (funcall ,callback msg)
              (setf (map-elt (listen-player-etc player) :requests)
                    (map-delete (map-elt (listen-player-etc player) :requests) request-id))))))
    (pcase-let* (((cl-struct listen-player (etc (map :network-process))) player)
                 (request-id (cl-incf (map-elt (listen-player-etc player) :request-id)))
                 (`(,command . ,args) command-args)
                 (json (json-encode `(("command" ,command ,@args)
                                      ("request_id" . ,request-id)))))
      (listen-debug :buffer "*listen-mpv*" (listen-player-process player) json)
      (process-send-string network-process json)
      (process-send-string network-process "\n")
      ;; TODO: Maybe check for success/error.
      (if then
          (progn
            (setf (map-elt (map-elt (listen-player-etc player) :requests) request-id)
                  (wrap-callback then))
            request-id)
        (let ((value :unknown))
          (setf (map-elt (map-elt (listen-player-etc player) :requests) request-id)
                (wrap-callback
                 (lambda (msg)
                   ;; Save the callback's value to the map so we can retrieve it.
                   (setf value (map-elt msg 'data)))))
          (accept-process-output (listen-player-process player) 0.05)
          ;; Return the then's value.
          value)))))

(cl-defmethod listen--seek ((player listen-player-mpv) seconds)
  "Seek PLAYER to SECONDS."
  (listen--send* player `("seek" ,seconds "absolute") :then #'ignore))

(cl-defmethod listen--volume ((player listen-player-mpv) &optional volume)
  "Return or set PLAYER's VOLUME.
VOLUME is an integer percentage."
  (pcase-let (((cl-struct listen-player max-volume) player))
    (if volume
        (progn
          (unless (<= 0 volume max-volume)
            (error "VOLUME must be 0-%s" max-volume))
          ;; We assume that the command will work, and we set the volume that is being set,
          ;; because the Transient description uses the value from the player slot, and the
          ;; callback can't make the Transient update itself.
          (listen-mpv--set-property player "volume" volume)
          (setf (listen-player-volume player) volume))
      (listen-player-volume player))))

(cl-defmethod listen-mpv--get-property ((player listen-player-mpv) property &key then)
  (listen--send* player `("get_property" ,property) :then then))

(cl-defmethod listen-mpv--set-property ((player listen-player-mpv) property value &key then)
  (listen--send* player `("set_property" ,property ,value) :then then))

(provide 'listen-mpv)

;;; listen-mpv.el ends here
