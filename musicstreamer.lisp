(in-package #:cloudradio)

(defclass mp3-streamer ()
  ((n     :initform 0)
   (phase :initform 0.0)))   

(defmethod mixalot:streamer-mix-into ((streamer mp3-streamer) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (n phase) streamer
    (loop for index upfrom offset
       repeat length
       with freq = (+ 200 (* n 50))
       with dp = (* 2.0 pi freq 1/44100)
       as sample = (round (* 5000 (sin phase)))
       do            
       (mixalot:stereo-incf (aref buffer index) (mixalot:mono->stereo sample))
       (incf phase dp))
    (when (= (incf n) 50)
      (mixalot:mixer-remove-streamer mixer streamer))))
