;;;; strifebarge.asd

(asdf:defsystem #:strifebarge
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
	       #:ironclad
               #:parenscript
               #:cl-css
               #:swank
               #:clsql)
  :components ((:file "package")
	       (:file "util")
	       (:file "model") (:file "board") (:file "game")
               (:file "strifebarge")))

