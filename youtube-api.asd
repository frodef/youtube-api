;;;; youtube.asd

(asdf:defsystem #:youtube-api
  :description "Describe youtube-api here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "youtube-api"))
  :depends-on (:str
	       :quri
	       :jonathan
	       :local-time))
