;;;; package.lisp

(defpackage #:strifebarge
  (:use #:cl #:cl-who #:cl-css #:clsql #:hunchentoot #:parenscript)
  (:import-from #:json #:encode-json-to-string #:decode-json-from-string)
  (:import-from #:cl-ppcre #:scan-to-strings)
  (:import-from #:ironclad 
   		#:encrypt-in-place #:decrypt-in-place #:make-cipher #:digest-sequence 
		#:octets-to-integer #:integer-to-octets
   		#:ascii-string-to-byte-array #:byte-array-to-hex-string)
  (:shadow #:get-time))

(in-package #:strifebarge)

;;;;;;;;;;;;;;;;;;;; config variable

(defparameter *server-port* 5050)
(defparameter *board-square-size* 35)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;