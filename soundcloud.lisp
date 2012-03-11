(in-package #:cloudradio)

(defparameter *soundcloud-base* "http://api.soundcloud.com/")
(defparameter *client-id* "f9acccf1e22d0a3b34b4ed42e260052b")
(defparameter *client-secret* "849059ac0d5373f85b9f77af59dc0853")

(defparameter *track-map* (make-hash-table :test 'eq))

(defparameter *default-parameter*
  `(("client_id" . ,*client-id*)))

(defparameter *default-track-parameters*
  '(
    ("types" . "original,live,remix")
    ("filter" . "public, streamable")
    ("duration[from]" . "60000") ; only tracks >= 1 minute
    ("duration[to]" . "600000") ; only tracks <= 10 minutes
    ))


(defun put-tracks (hashtable tracks)
  (mapc (lambda (track) (put-track hashtable track)) tracks))

(defun put-track (hashtable track)
  "puts track into hashmap"
  (let ((hash-key (slot-value track 'id)))
    (setf (gethash hash-key hashtable) track)))

(defun query-tracks (&optional parameters '())
  "returns a list of track objects"
  (mapcar #'make-track 
          (query-soundcloud "tracks" :parameters parameters)))

(defun make-uri (base parameters)
  (concatenate 'string base
               (format nil "?~{~{~a~^=~}~^\&~}"
                       (mapcar (lambda (cons)
                                 (list (car cons) (cdr cons)))
                               parameters))))

(defun download-track (track)
  "Downloads a track from soundcloud"
  (multiple-value-bind (body status headers uri stream)
      (drakma:http-request (make-uri (stream-url track) *default-parameter*) :preserve-uri t :want-stream t)
    (if (= status 200)
        (let ((file-path (concatenate 'string "/home/willi/mp3/"
                                      (format nil "~D" (id track))
                                      ".mp3")))
          (with-open-file (os file-path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
              (let ((fd (sb-sys:fd-stream-fd (chunga:chunked-stream-stream (slot-value stream 'stream)))))
                (mixalot:mixer-add-streamer (mixer *player*) (mixalot-mp3:make-mp3-stream-streamer fd))))))
        (error "could not download track"))))

(defun query-soundcloud (location &key (parameters '()))
  "sends a request to the specified location on soundcloud (tracks, playlists, ...) and returns the data as assoc list"
  (let ((endpoint (concatenate 'string *soundcloud-base* location ".json")))
    (multiple-value-bind (body status headers)
        (let ((params
               (remove-duplicates
                (mapcar (lambda (item)
                          (cons (car item) 
                                (princ-to-string (cdr item))))
                        (append *default-parameter* parameters)))))
          (drakma:http-request endpoint  
                               :method :get
                               :force-binary t
                               :parameters params))
      (let ((body (babel:octets-to-string body)))
        (if (= status 200)
            (let ((json-response
                   (json::decode-json-from-string body)))
              json-response))))))

(defun get-track-count ()
  "returns the (approximate) number of tracks in the soundcloud"
  (id (car (query-tracks '(("order" . "latest")
                           ("limit" . 1))))))



(defun get-random-tracks (&optional (limit 200) (track-count (get-track-count)))
  "returns a list of (hopefully) random tracks, used to kickstart a
profile. No guarantee is given on the number of tracks returned."
  (query-tracks `(("duration[from]" . 60000)
                  ("duration[to]" . 600000)
                  ("limit" . ,limit)
                  ("types" . "original,live")
                  ("filter" . "streamable")
                  ("ids" . ,(reduce
                             (lambda (x y)
                               (concatenate 'string x "," y))
                             (loop repeat 200
                                collect (princ-to-string
                                         (random track-count))))))))
