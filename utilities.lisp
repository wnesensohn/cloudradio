(in-package #:cloudradio)

(defparameter *split-genre-regex* "[ \.,&-/]")

(defun get-genre-list (genre-string)
  "splits a string which represents a genre into a list of strings"
  (remove-if (lambda (sub)
               (= 0 (length sub)))
             (cl-ppcre:split *split-genre-regex* (string-upcase genre-string))))
  
