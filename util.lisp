(in-package :strifebarge)

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun range (a b)
  (loop for i from a to b collect i))

(defmacro web-folders (&body body)
  "Sets up folder dispatchers for the given folders"
  `(progn ,@(mapcar #'(lambda (f) 
			`(push (create-folder-dispatcher-and-handler ,(format nil "/~a/" f) ,(format nil "~a/" f)) *dispatch-table*))
		    body)))

(defmacro html-to-stout (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(with-html-output-to-string (*standard-output*) ,@body))

(defmacro define-ship (name length &optional (width 1))
  `(defclass ,name (ship) 
     ((len :initform ,length)
      (wid :initform ,width)
      (space-count :initform ,(* length width)))))