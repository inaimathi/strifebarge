;;;; package.lisp

(defpackage #:strifebarge
  (:use #:cl #:cl-who #:clsql #:hunchentoot #:parenscript)
  (:import-from #:swank #:find-definition-for-thing)
  (:import-from #:ironclad 
   		#:encrypt-in-place #:decrypt-in-place #:make-cipher #:digest-sequence 
		#:octets-to-integer #:integer-to-octets
   		#:ascii-string-to-byte-array #:byte-array-to-hex-string)
  (:shadow #:get-time))

(in-package #:strifebarge)

(defparameter *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 5050))) 