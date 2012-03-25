(in-package #:cloudradio)

;;; This module represents profiles which define the music taste of a
;;; user

(defparameter *length-resolution* 1000) ; resolution of length
                                        ; parameter, in ms
(defparameter *maximal-length* 600000)  ; maximal length in ms
(defparameter *bpm-resolution* 1)
(defparameter *maximal-bpm* 250)

(defclass rating ()
  ((upvotes :reader rating-upvotes :initform 0 :initarg :upvotes)
   (downvotes :reader rating-downvotes :initform 0 :initarg :downvotes)))

(defun rating= (lhs rhs)
  (and (= (rating-upvotes lhs) (rating-upvotes rhs)) (= (rating-downvotes lhs) (rating-downvotes rhs))))

(defclass music-genre ()
  ((name :reader name :initarg :name)
   (rating :reader rating :initform (make-instance 'rating))
   (confidence :reader confidence :initform 0)))

(defmemo:defmemo compute-rating (upvotes downvotes)
  (let ((z 1)
        (n (+ upvotes downvotes)))
    (if (= n 0)
        0
        (let ((phat (/ (* upvotes) n)))
          (/ (- (+ phat (/ (* z z) (* n 2))) (* z (sqrt (/ (+ (* phat (- 1 phat)) (/ (* z z) (* 4 n))) n)))) (+ 1 (/ (* z z) n)))))))

(defgeneric get-rating (rating))

(defmethod get-rating ((rating rating))
  (with-slots (upvotes downvotes) rating
    (compute-rating upvotes downvotes)))


(defgeneric vote-up (type profile attribute))
(defgeneric vote-down (type profile attribute))

(defclass music-profile ()
  ((genres
    :reader genres
    :initform '())
   (length-histogram
    :reader length-histogram
    :initform (make-array (round (/ *maximal-length* *length-resolution*))))
   (bpm-histogram
    :reader bpm-histogram
    :initform (make-array (round (/ *maximal-bpm* *bpm-resolution*))))
   (created-histogram
    :reader created-histogram) ; not yet implemented
   ))

(defmethod vote-up ((type (eql :genre)) (profile music-profile) (name string))
  (vote-genre profile name 'upvotes))

(defmethod vote-down ((type (eql :genre)) (profile music-profile) (name string))
  (vote-genre profile name 'downvotes))

(defun vote-genre (profile name method)
    (let* ((sequence (genres profile))
         (attribute (find name sequence :test (lambda (r l) (string-equal (name l) r)))))
    (when (not attribute)
      (setf attribute (make-instance 'music-genre :name name))
      (setf (slot-value (rating attribute) method) 0)
      (push attribute (slot-value profile 'genres)))
    (incf (slot-value (rating attribute) method))))
