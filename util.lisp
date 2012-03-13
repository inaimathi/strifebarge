(in-package :strifebarge)

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun range (a b)
  (loop for i from a to b collect i))

(defun board-scale (num) (* *board-square-size* num))

(defun mapcan-f (fn a-list)
  "Functional implementation of unary mapcan"
  (loop for i in a-list append (funcall fn i)))

(defun shuffle (a-list)
  (let ((l (copy-seq a-list)))
    (sort l #'> :key (lambda (n) (declare (ignore n)) (random 1.0)))))

(defmacro web-folders (&body body)
  "Sets up folder dispatchers for the given folders"
  `(progn ,@(mapcar #'(lambda (f) 
			`(push (create-folder-dispatcher-and-handler ,(format nil "/~a/" f) ,(format nil "~a/" f)) *dispatch-table*))
		    body)))

(defmacro redirect-unless (predicate &optional (target "/"))
  `(unless ,predicate (redirect ,target)))

(defmacro html-to-stout (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(with-html-output-to-string (*standard-output*) ,@body))

(defun instance-to-id (instance)
  (aref (nth-value 1 (scan-to-strings "{\(.*?\)}" (format nil "~a" instance))) 0))

(defun take (num a-list)
  (if (> (length a-list) num)
      (subseq a-list 0 num)
      a-list))

(defmacro define-ship (name length &optional (width 1))
  `(defclass ,name (ship) 
     ((len :initform ,length)
      (wid :initform ,width)
      (space-count :initform ,(* length width)))))