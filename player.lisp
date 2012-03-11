(in-package #:cloudradio)


;;(defvar *mixer* (mixalot:create-mixer))

(mixalot:main-thread-init)

(mpg123:ensure-libmpg123-initialized)

(defclass mp3-player ()
  ((stopped :accessor stopped :initform nil)
   (mixer :accessor mixer :initform (mixalot:create-mixer))))

(defvar *player* (make-instance 'mp3-player))

;(defmethod mixalot:streamer-cleanup :after (stream mixer)
;  (if (not (stopped *player*))
;      (let ((track (alexandria:random-elt (get-random-tracks))))
;        (download-track track)
;        (play-track track))))

(defun skip-track ()
  (mixalot:mixer-remove-all-streamers (mixer *player*)))

(defun stop-player ()
  (setf (stopped *player*) t)
  (skip-track))

(defun play-track (track)
  (mixalot:mixer-add-streamer (mixer *player*) (mixalot-mp3:make-mp3-streamer (namestring (get-path track)))))

