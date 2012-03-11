(in-package #:cloudradio)


;;; This module performs the rating of tracks, based on a given
;;; profile

(defgeneric rate-track (track profile rating)
  (:documentation "rates a track"))


(defmethod rate-track ((track track) (profile music-profile) (rating real))
  "rates the track in the easiest way possible"
  (let ((genre-count (length (genre track)))
        (genres (genres profile)))
    (mapc (lambda (genre)
            (if (null (gethash genre (genres profile)))
                (setf (gethash genre (genres profile)) (make-instance 'music-genre :name genre)))
            (incf (slot-value (gethash genre (genres profile)) 'rating) (/ rating genre-count))
            (incf (slot-value (gethash genre (genres profile)) 'confidence) (/ 1 genre-count))
            ) (genre track))))

