;;;; strifebarge.asd

(asdf:defsystem #:strifebarge
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
	       #:cl-json
	       #:ironclad
               #:parenscript
               #:cl-css
               #:swank
               #:clsql)
  :components ((:file "package")
	       (:file "util") (:file "js-macros")
	       (:file "model") (:file "space") (:file "board") (:file "game")
               (:file "strifebarge")
	       (:file "start")))

