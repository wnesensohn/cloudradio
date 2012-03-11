;;;; cloudradio.asd

(asdf:defsystem #:cloudradio
  :serial t
  :depends-on (
               #:thread-pool
               #:bordeaux-threads
               #:arnesi
               #:cl-json
               #:drakma
               #:babel
               #:mpg123-ffi
               #:mixalot
               #:mixalot-mp3
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

