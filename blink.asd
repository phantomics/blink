;;;; blink.asd

(asdf:defsystem #:blink
  :description "Blink compiles a version of the K programming language to Common Lisp."
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "utilities")
               (:file "spec")))
