(in-package #:cloudradio)


(defun download-random-track () 
  (let ((track (car (get-random-tracks 1))))
    (print (concatenate 'string (stream-url track) "?client_id=" *client-id*))
    (multiple-value-bind (body status headers uri stream)
        (drakma:http-request (stream-url track) :parameters `(("client_id" . ,*client-id*)) :redirect nil)
      (close stream)
      (let ((real-location (cdr (assoc :location headers))))
        (multiple-value-bind (body status headers uri stream)
            (drakma:http-request real-location :want-stream t)
          (if (= status 200)
              (let ((file-path (concatenate 'string "/home/willi/mp3/"
                                            (format nil "~D" (id track))
                                            ".mp3")))
                (with-open-file (os file-path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
                  (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
                    (do ((nread (read-sequence buffer stream)
                                (read-sequence buffer stream))
                         (total 0
                                (+ total nread)))
                        ((zerop nread) total)
                      (write-sequence buffer os :end nread))))
                (print file-path)))
          (close stream))))))

(defun play-random-tracks ()
  (let ((track (car (get-random-tracks 1))))
    (download-track track)
    (loop 
       (format t "Now playing: ~D (Genres: ~D)~%"
               (title track)
               (if (null (genre track)) "no genre specified"
                   (reduce
                    (lambda (x y)
                      (concatenate 'string x " " y)) (genre track))))
       (download-track track)
       (sleep 10)
       (setf track (car (get-random-tracks 1)))
       (mixalot:mixer-remove-all-streamers (mixer *player*))
       )))


(defun url-decode (string)
  "Returns a URL-decoded version of the STRING"
  (with-output-to-string (out) 
    (with-input-from-string (string-stream string)
      (do ((chr (read-char string-stream nil)
                (read-char string-stream nil)))
          ((null chr))
        (if (char-equal #\% chr)
            (format out "~c" (code-char
                              (parse-integer
                               (format nil "~c~c"
                                       (read-char string-stream nil)
                                       (read-char string-stream nil))
                               :radix 16)))
            (format out "~c" chr))))))
