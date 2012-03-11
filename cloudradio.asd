;;;; cloudradio.asd

(asdf:defsystem #:cloudradio
  :serial t
  :depends-on (#:bordeaux-threads
               #:drakma
               #:babel
               #:cl-json
               #:mpg123-ffi
               #:mixalot
               #:mixalot-mp3
               #:thread-pool
               #:trivial-http
               )
  :components (
               (:file "mp3-stream")
               (:file "package")
               (:file "profile")
               (:file "cloudradio")
               (:file "utilities")
               (:file "soundcloud")
               (:file "tracks")
               (:file "rating")
               (:file "musicstreamer")
               (:file "player")
               (:file "tests")
               ))

