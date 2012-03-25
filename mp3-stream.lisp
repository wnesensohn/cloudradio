;;;; Mixalot MP3 streaming

;;;; Copyright (c) 2009 Andy Hefner

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sellcopies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject
;;;;  to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(defpackage :cloudradio-mp3
  (:use :common-lisp :cffi :mixalot-ffi-common :mixalot-strings-common :mixalot :mpg123)
  (:export #:mp3-stream-streamer
           #:make-mp3-stream-streamer
           #:mp3-streamer-release-resources

           #:mpg123-getstate
           
           #:MPG123_ACCURATE
           #:MPG123_BUFFERFILL
           #:MPG123_FRANKENSTEIN))

(in-package #:cloudradio-mp3)

(defcenum mpg123-state
  (:MPG123_ACCURATE 1)
  (:MPG123_BUFFERFILL 2)
  (:MPG123_FRANKENSTEIN 3))


(defcfun (%mpg123-getstate "mpg123_getstate") :int
  (mh mpg123::handleptr)
  (key mpg123-state)
  (val (:pointer :long))
  (fval (:pointer :double)))

(defun mpg123-getstate (mh key)
  (with-foreign-objects ((val :long)
                         (fval :double))
    (check-mh-error "mpg123 get state" mh
                    (%mpg123-getstate mh key val fval))
    (values (mem-ref val :long)
            (mem-ref fval :double))))

(defclass buffer ()
  ((buffers :initform (make-array 8 :initial-contents (loop repeat 8 collect (make-array (* 2 1024) :element-type '(unsigned-byte 8)))) :accessor buffers)
   (read-pointer :initform 0 :accessor read-pointer)
   (write-pointer :initform 0 :accessor write-pointer)
   (buffer-ready :initform nil :accessor buffer-ready)
   (buffer-length :initform nil :reader buffer-length)))

(defun write-buffer (buffer)
  (aref (buffers buffer) (mod (write-pointer buffer) (buffer-length buffer))))

(defun read-buffer (buffer)
  (aref (buffers buffer) (mod (read-pointer buffer) (buffer-length buffer))))

(defun rw-equal (buffer)
  (let ((length (buffer-length buffer)))
    (= (mod (read-pointer buffer) length) (mod (+ 0 (write-pointer buffer)) length))))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (setf (slot-value buffer 'buffer-length) (length (buffers buffer))))

(defun fill-buffer (streamer &optional first-run)
  (with-slots (finished buffer stream) streamer
    (unless finished
                                        ;(format t "rb-eq: ~A~%" (eq (read-pointer buffer) (write-pointer buffer)))
      (loop until (and (not first-run) (rw-equal buffer))
         do
         (format t "fill rp: ~A wp: ~A~%" (read-pointer buffer) (write-pointer buffer))
         (let* ((write-buffer (write-buffer buffer))
                (position (read-sequence write-buffer stream))
                (max-length (length write-buffer)))

           (incf (write-pointer buffer))
           (setf first-run nil)
           
           (when (/= position max-length)
             (setf (finished streamer) t)
             (return-from fill-buffer t))))
      (setf (buffer-ready buffer) t))
    )
  nil)

(defun advance-buffer (buffer)
  (incf (read-pointer buffer)))

(defclass mp3-stream-streamer ()
  ((handle      :reader mpg123-handle :initarg :handle)
   (sample-rate :reader mp3-sample-rate :initarg :sample-rate)
   (output-rate :reader mp3-output-rate :initarg :output-rate)
   (stream    :initform nil :initarg stream)
   (buffer    :initform (make-instance 'buffer) :accessor buffer)
   (bufferthread :reader bufferthread)
   (sndbuffer :initform nil :accessor sndbuffer)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)
   (finished :initform nil :accessor finished)))

(defun open-mp3-stream (&key (output-rate 44100))
  "Open an MP3 file from a stream, forcing the output format to 16 bit,
stereo, resampling to the requested rate, and returning an
mpg123_handle pointer if successful."
  (ensure-libmpg123-initialized)
  (let (handle uhandle rate)
    (unwind-protect
         (with-foreign-object (err :int)
           (setf uhandle (mpg123-new (null-pointer) err))
           (check-mpg123-plain-error "mpg123-new" (mem-ref err :int))
           (mpg123-param uhandle :add-flags MPG123_QUIET 0.0d0)
           (check-mh-error "Clear default formats" uhandle (mpg123-format-none uhandle))
           (check-mh-error "Configure output format" uhandle
                           (mpg123-format uhandle output-rate 2 MPG123_ENC_SIGNED_16))
           (mpg123-param uhandle :force-rate output-rate 0.0d0)
           (check-mh-error "Open mp3 stream" uhandle (mpg123-open-feed uhandle))

           ;; The library wants see the stream format before we begin decoding.
           ;;(setf rate (mpg123-getformat uhandle))

           (rotatef handle uhandle))
      (when uhandle (mpg123-close uhandle)))
    (values handle rate)))

(defun mp3-stream-streamer-release-resources (mp3-stream)
  "Release foreign resources associated with the mp3-stream."
  (with-slots (handle stream bufferthread) mp3-stream
    (when handle
      (mpg123-close handle)
      (mpg123-delete handle)
      (setf handle nil)
      (if (and (bordeaux-threads:threadp bufferthread) (bordeaux-threads:thread-alive-p bufferthread))
          (bordeaux-threads:destroy-thread bufferthread))
      (close stream))))

(defmethod streamer-cleanup ((stream mp3-stream-streamer) mixer)
  (declare (ignore mixer))
  (mp3-stream-streamer-release-resources stream))

(defun make-mp3-stream-streamer
    (stream &rest args
     &key
     (output-rate 44100)
     (class 'mp3-stream-streamer)
     &allow-other-keys)
  "Create an mp3 audio stream from a stream, raising an mpg123-error if
the file cannot be opened or another error occurs."
  (multiple-value-bind (handle sample-rate)
      (open-mp3-stream :output-rate output-rate)
    (remf args :class)
    (let ((streamer (apply #'make-instance
                           class
                           :handle handle
                           :sample-rate sample-rate
                           :output-rate output-rate
                           'stream stream
                           args)))
      (fill-buffer streamer t)
      (setf (slot-value streamer 'bufferthread)
            (bordeaux-threads:make-thread  (lambda ()
                                             (loop until
                                                  (finished streamer)
                                                  do 
                                                  (fill-buffer streamer)
                                                  (sleep 0.01))
                                             (format t "buffer fill finished~%")) :name "buffer fill"))
      streamer)))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate) stream
    (when seek-to
      (mpg123-seek handle seek-to :set)
     (setf seek-to nil
           position (floor (* output-rate (mpg123-tell handle))
                           sample-rate)))))

(defmethod streamer-mix-into ((streamer mp3-stream-streamer) mixer mix-buffer offset length time)

  (update-for-seek streamer)
  (with-slots (buffer handle stream) streamer

    (loop until (buffer-ready buffer)
       do
       (format t "waiting...~%")
       (sleep 0.1))

    (format t "rp: ~A wp: ~A read: ~A~%" (read-pointer buffer) (write-pointer buffer) (aref (read-buffer buffer) 3))

                                        ;(update-for-seek streamer)

                                        ;(error "a")

                                        ;(format t "getstate: ~A~%" (mpg123-getstate handle :MPG123_ACCURATE))

    
    (with-array-pointer (inmem (read-buffer buffer))
      (with-foreign-object (done-pt 'mpg123::size_t)

        (let* ((max-length (* 4 4092))
               (inmem-length (length (read-buffer buffer)))
               (outmem-buffer (or (sndbuffer streamer)
                                  (setf (sndbuffer streamer)
                                        (make-array max-length :element-type 'stereo-sample)))))
          (with-array-pointer (outmem outmem-buffer)

            (if (finished streamer)
                (setf inmem-length 0))
            
            (loop with end-output-index = (the array-index (+ offset length))
               with output-index = offset
               with err = MPG123_NEW_FORMAT ;
               with samples-read = 0
               with chunk-size = 0
               while (< output-index end-output-index) do

               (setf chunk-size (min max-length (- end-output-index output-index))
                     err (mpg123-decode handle inmem inmem-length outmem (* 4 chunk-size) done-pt)
                     samples-read (the array-index (ash (mem-ref done-pt 'mpg123::size_t) -2)))

               (when (= err MPG123_NEW_FORMAT)
                 (loop-finish))

               (when (= 0 (mem-ref done-pt 'mpg123::size_t))
                 (setf err MPG123_DONE)
                 (loop-finish))

               ;; Mix into buffer
               (loop for out-idx upfrom (the array-index output-index)
                  for in-idx upfrom 0
                  repeat samples-read

                  do
                  (stereo-mixf (aref mix-buffer out-idx)
                               (aref (sndbuffer streamer) in-idx)))
                 
               (incf output-index samples-read)
               (incf (slot-value streamer 'position) samples-read)

               finally
               (cond
                 ((= err MPG123_DONE)   ; End of stream.
                  (mixer-remove-streamer mixer streamer))
                 ((= err MPG123_NEW_FORMAT) 
                  t)
                 ((= err MPG123_NEED_MORE) ; shouldn't happen
                  t)
                 ((/= err 0)            ; Other error?
                  (format *trace-output* "~&~A (~A): error ~A: ~A~%"
                          streamer
                          (slot-value streamer 'stream)
                          err
                          (mpg123-strerror handle))
                  (mixer-remove-streamer mixer streamer))))
            
            (advance-buffer buffer)))))))


;;; Seek protocol

(defmethod streamer-seekable-p ((stream mp3-stream-streamer) mixer)
  (declare (ignore mixer)
           (ignore stream))
  nil)

(defmethod streamer-length ((stream mp3-stream-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (length) stream
    length))

(defmethod streamer-seek ((stream mp3-stream-streamer) mixer position
                          &key &allow-other-keys)
  (declare (ignore mixer))
  (with-slots (seek-to sample-rate output-rate) stream
    (setf seek-to (floor (* sample-rate position) output-rate)))
  (values))

(defmethod streamer-position ((stream mp3-stream-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (position) stream
    position))
