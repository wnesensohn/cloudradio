(in-package #:cloudradio)

(defvar *music-profile* (make-instance 'music-profile))

;; thread-pool for fetching tracks
;; maybe do that with eager-future2
(defvar *fetch-threadpool* (thread-pool:make-thread-pool 4))


;; thread-pool for rating tracks
(defvar *rate-threadpool* (thread-pool:make-thread-pool))

