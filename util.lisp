(in-package :strifebarge)

;;;;;;;;;; basic list operations
(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun range (a b)
  "Returns the list of numbers from a to b inclusive"
  (loop for i from a to b collect i))

(defun mapcan-f (fn a-list)
  "Functional implementation of unary mapcan"
  (loop for i in a-list append (funcall fn i)))

(defun shuffle (a-list)
  "Returns a randomly sorted copy of the given list"
  (let ((l (copy-seq a-list)))
    (sort l #'> :key (lambda (n) (declare (ignore n)) (random 1.0)))))

(defun take (num a-list)
  "Returns the first num elements of a-list.
If (length a-list) is shorter than num, returns a-list instead."
  (if (> (length a-list) num)
      (subseq a-list 0 num)
      a-list))

;;;;;;;;;; cl-who/hunchentoot shortcuts
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

;;;;;;;;;; CLOS/object operations
(defmacro define-ship (name length &optional (width 1))
  "Shortcut for defining a ship type. Automatically keeps
space-count consistent with len and wid slots."
  `(defclass ,name (ship) 
     ((len :initform ,length)
      (wid :initform ,width)
      (space-count :initform ,(* length width)))))

(defun instance-to-id (instance)
  (aref (nth-value 1 (scan-to-strings "{\(.*?\)}" (format nil "~a" instance))) 0))

(defun slot-names (class)
  "Portable slot-names function for CLOS instances."
  (mapcar #'slot-definition-name
          (class-slots class)))

(defun map-slots (fn instance)
  "Portable unary map function for CLOS instances.
Takes a binary function, and passes it ([slot-name] [slot-value]) 
for each slot in instance."
  (loop for slot-name in (slot-names (class-of instance))
        collect (funcall fn slot-name 
			 (when (slot-boundp instance slot-name) 
			   (slot-value instance slot-name)))))

;;;;;;;;;; flow control
(defmacro redirect-unless (predicate &optional (target "/"))
  `(unless ,predicate (redirect ,target)))