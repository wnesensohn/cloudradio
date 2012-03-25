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

(defun query-tracks (&optional parameters)
  "returns a list of track objects"
  (mapcar #'make-track 
          (query-soundcloud "tracks" :parameters parameters)))

(defun make-uri (base parameters)
  (concatenate 'string base
               (format nil "?~{~{~a~^=~}~^\&~}"
                       (mapcar (lambda (cons)
                                 (list (car cons) (cdr cons)))
                               parameters))))

(defun sc-play-track (track)
  "plays a track from soundcloud. returns immediatly"
  (multiple-value-bind (body status headers uri stream)
      (http-request (make-uri (stream-url track) *default-parameter*) :preserve-uri t :want-stream t)
    (declare (ignore body headers uri))
    (if (= status 200)
        (mixalot:mixer-add-streamer (mixer *player*) (cloudradio-mp3:make-mp3-stream-streamer stream))
        (error "could not stream track"))))

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
          (http-request endpoint  
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
  (remove-if-not #'genre 
                 (query-tracks `(("duration[from]" . 60000)
                                 ("duration[to]" . 600000)
                                 ("limit" . ,limit)
                                        ;("genres" . "Rock, Pop")
                                        ;("types" . "original,live")
                                 ("filter" . "streamable")
                                 ("ids" . ,(reduce
                                            (lambda (x y)
                                              (concatenate 'string x "," y))
                                            (loop repeat 500
                                               collect (princ-to-string
                                                        (random track-count)))))))))
