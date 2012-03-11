(in-package #:cloudradio)

;;; This module represents profiles which define the music taste of a
;;; user

(defparameter *length-resolution* 1000) ; resolution of length
                                        ; parameter, in ms
(defparameter *maximal-length* 600000)  ; maximal length in ms
(defparameter *bpm-resolution* 1)
(defparameter *maximal-bpm* 250)

(defclass music-genre ()
  ((name :reader name :initarg :name)
   (rating :reader rating :initform 0.5)
   (confidence :reader confidence :initform 0)))


(defclass music-profile ()
  ((genres
    :reader genres
    :initform (make-hash-table :test 'equal))
   (length-histogram
    :reader length-histogram
    :initform (make-array (round (/ *maximal-length* *length-resolution*))))
   (bpm-histogram
    :reader bpm-histogram
    :initform (make-array (round (/ *maximal-bpm* *bpm-resolution*))))
   (created-histogram
    :reader created-histogram) ; not yet implemented
   ))
