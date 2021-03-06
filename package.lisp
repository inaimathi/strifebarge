;;;; package.lisp

(defpackage #:strifebarge
  (:use #:cl #:cl-who #:cl-css #:clsql #:hunchentoot #:parenscript #:cl-actors)
  (:import-from #:cl-actors #:self)
  (:import-from #:json #:encode-json-to-string #:decode-json-from-string)
  (:import-from #:cl-ppcre #:scan-to-strings)
  (:import-from #:ironclad 
   		#:encrypt-in-place #:decrypt-in-place #:make-cipher #:digest-sequence 
		#:octets-to-integer #:integer-to-octets
   		#:ascii-string-to-byte-array #:byte-array-to-hex-string)
  (:shadowing-import-from 
   #+openmcl-native-threads #:ccl
   #+cmu #:pcl
   #+sbcl #:sb-pcl
   #+lispworks #:hcl
   #+allegro #:mop
   #+clisp #:clos
   #:class-slots #:slot-definition-name)
  (:shadow #:get-time))

(in-package #:strifebarge)

;;;;;;;;;;;;;;;;;;;; config variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *server-port* 5050)
(defparameter *board-square-size* 35)
(defparameter *game-ticker-frequency* 30 
  "How often, in seconds, the game clock should tick")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun board-scale (num)
  "Used to scale ship and space representations."
  (* *board-square-size* num))