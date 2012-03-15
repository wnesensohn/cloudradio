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


(defpackage :mixalot-mp3
  (:use :common-lisp :cffi :mixalot :mpg123)
  (:export #:mp3-stream-streamer
           #:make-mp3-stream-streamer
           #:mp3-streamer-release-resources))

(in-package #:mixalot-mp3)

(defclass buffer ()
  ((buffers :initform (make-array 10 :initial-element (make-array (* 2 4096) :element-type '(unsigned-byte 8))) :accessor buffers)
   (read-pointer :initform 0 :accessor read-pointer)
   (write-pointer :initform 1 :accessor write-pointer)
   (buffer-length :initform nil :reader buffer-length)))

(defun write-buffer (buffer)
  (aref (buffers buffer) (write-pointer buffer)))

(defun read-buffer (buffer)
  (aref (buffers buffer) (read-pointer buffer)))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  (setf (slot-value buffer 'buffer-length) (length (buffers buffer))))

(defun fill-buffer (buffer stream)
  (loop until (= (read-pointer buffer) (write-pointer buffer))
     do
     (let ((position (read-sequence (write-buffer buffer) stream))
           (max-length (length (write-buffer buffer))))

       ;(format t "first elem: ~A~%" (aref (write-buffer buffer) 0))

       (if (= position 0) (return nil))
       (setf (write-pointer buffer) (mod (1+ (write-pointer buffer)) (buffer-length buffer)))
       (if (/= position max-length) (return nil))))
  t)

(defun advance-buffer (buffer)
  (setf (read-pointer buffer) (mod (1+ (read-pointer buffer)) (buffer-length buffer))))

(defclass mp3-stream-streamer ()
  ((handle      :reader mpg123-handle :initarg :handle)
   (sample-rate :reader mp3-sample-rate :initarg :sample-rate)
   (output-rate :reader mp3-output-rate :initarg :output-rate)
   (stream :initform nil :initarg stream)
   (buffer    :initform (make-instance 'buffer) :accessor buffer)
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
  (with-slots (handle stream) mp3-stream
    (when handle
      (mpg123-close handle)
      (mpg123-delete handle)
      (setf handle nil)
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
      streamer)))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate) stream
    (when seek-to
      (mpg123-seek handle seek-to :set)
     (setf seek-to nil
           position (floor (* output-rate (mpg123-tell handle))
                           sample-rate)))))

(defmethod streamer-mix-into ((streamer mp3-stream-streamer) mixer mix-buffer offset length time)
  (declare (optimize (speed 3) (safety 0)))

                                        ;(update-for-seek streamer)
  (with-slots (buffer handle stream) streamer
    
    (fill-buffer buffer stream)
    
    (with-array-pointer (inmem (read-buffer buffer))
      (with-foreign-object (done 'mpg123::size_t)

        (let* ((max-length (* 10 4092))
               (inmem-length (length (read-buffer buffer)))
               (outmem-buffer (or (sndbuffer streamer)
                                  (setf (sndbuffer streamer)
                                        (make-array max-length :element-type 'stereo-sample)))))
          (with-array-pointer (outmem outmem-buffer)

            (loop with end-output-index = (the array-index (+ offset length))
               with output-index = offset
               with err = 0
               with samples-read = 0
               with chunk-size = 0
               while (< output-index end-output-index) do

               (setf chunk-size (min max-length (- end-output-index output-index))
                     err (mpg123-decode handle inmem inmem-length outmem (* 4 chunk-size) done)
                     samples-read (the array-index (ash (mem-ref done 'mpg123::size_t) -2)))

               ;;(format t "first element: ~A samples read: ~A~%" (aref (read-buffer buffer) 0) samples-read)

               (advance-buffer buffer)
               (when (not (zerop err)) (loop-finish))

               ;; Mix into buffer
               (loop for out-idx upfrom (the array-index output-index)
                  for in-idx upfrom 0
                  repeat samples-read
                   
                  do
                  (stereo-mixf (aref mix-buffer out-idx)
                               (aref (sndbuffer streamer) in-idx)))
                 
               (incf output-index samples-read)
               (incf (slot-value streamer 'position) samples-read)

                                        ;(error "a")

               finally
               ;(format t "error: ~A samples read: ~A chunk size: ~A~%" err samples-read (mem-ref done 'mpg123::size_t))
               (cond
                 ((= err MPG123_DONE)   ; End of stream.
                  (mixer-remove-streamer mixer streamer))
                 ((= err MPG123_NEW_FORMAT)
                  t)
                 ((= err MPG123_NEED_MORE)
                  t)
                 ((/= err 0)            ; Other error?
                  (format *trace-output* "~&~A (~A): error ~A: ~A~%"
                          streamer
                          (slot-value streamer 'stream)
                          err
                          (mpg123-strerror handle))
                  (mixer-remove-streamer mixer streamer))))))))))

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
