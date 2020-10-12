;;;; game2.asd

(asdf:defsystem #:game2
  :description "Describe game2 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("trivial-gamekit" "anaphora")
  :components ((:file "package")
               (:file "game2")))
