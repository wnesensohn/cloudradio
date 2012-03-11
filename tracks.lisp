(in-package #:cloudradio)

(defgeneric get-rating (profile track)
  (:documentation "given a profile, returns the rating (as real) of a track"))
(defgeneric get-playable (track)
  (:documentation "returns a generalized boolean if the track is playable"))
(defgeneric get-path (track)
  (:documentation "returns the path of the downloaded track"))

(defclass track ()
  ((id :reader id :initarg :id)
   (genre :reader genre :initarg :genre)
   (title :reader title :initarg :title)
   (bpm :reader bpm :initarg :bpm)
   (duration :reader duration :initarg :duration)
   (stream-url :reader stream-url :initarg :stream-url)
   (download-url :reader download-url :initarg :download-url)
   ))

(defmethod get-playable ((track track))
  (probe-file (concatenate 'string "/home/willi/mp3/"
                           (format nil "~D" (id track))
                           ".mp3")))

(defmethod get-path ((track track))
  (get-playable track))

(defun make-track (properties)
  (labels ((get-value (name) (cdr (assoc name properties))))
    (make-instance 'track
                   :id (get-value :id)
                   :genre (get-genre-list (get-value :genre))
                   :title (get-value :title)
                   :duration (get-value :duration)
                   :bpm (get-value :bpm)
                   :stream-url (get-value :stream--url)
                   :download-url (get-value :download--url)
                   )))

(defmethod get-rating ((profile music-profile) (track track))
  (reduce '+
          (mapcar (lambda (genre-name)
                    (let ((genre (gethash genre-name (genres profile))))
                      (if (null genre) 0
                          (* (rating genre) (confidence genre)))))
                  (genre track))))
