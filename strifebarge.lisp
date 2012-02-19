;;;; strifebarge.lisp
(in-package #:strifebarge)

;;;;;;;;;; classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ship ()
  ((space-count :reader space-count :initarg :space-count)
   (damage :accessor damage :initform 0)
   (coords :reader coords :initarg :coords)
   (player :reader player :initarg :player)))

(defclass carrier (ship) ((space-count :initform 5)))
(defclass cruiser (ship) ((space-count :initform 3)))
(defclass destroyer (ship) ((space-count :initform 2)))

(defclass player ()
  ((score :accessor score :initarg :score)
   (sunken :accessor sunken :initarg :sunken)
   (ships :accessor ships
	  :initform (list (make-instance 'carrier)
			  (make-instance 'cruiser)
			  (make-instance 'destroyer)))))

(defclass board ()
  ((width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (spaces :reader spaces :initarg :spaces)))

(defclass game ()
  ((board :accessor board :initarg :board)
   (players :accessor players :initarg :players)
   (turn-stack :accessor turn-stack :initarg :turn-stack)
   (moves :accessor moves :initarg :moves :initform nil)
   (history :accessor history :initform nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *empty-space* 'o)

(defmethod generate ((b board) &rest ships)
  (make-instance '))

(defmethod try-position ((s ship) width height)
  (let ((x (random (- width (space-count s))))
	(y (random (- height (space-count s))))
	(direction (pick '(:vertical :horizontal))))
    (loop for i from 0 to (- (space-count s) 1)
	  if (eq :vertical direction)
	    collect (list x (+ i y))
	  else
	    collect (list (+ i x) y))))

(defmethod pt ((b board) x y) (nth x (nth y (spaces b))))

(defmethod fire ((g game) x y)
  (let ((result  (if (eq *empty-space* (pt (board g) x y)) :miss :hit)))
    (setf (nth x (nth y (moves g))) result)
    result))

(defun empty-board (width height &optional (empty-space *empty-space*))
  "Returns a collection of [height] rows, each of length [width].
Each cell is independant (which is why the board isn't built with make-list)"
  (loop for x from 0 to height collect (loop for y from 0 to width collect empty-space)))

(defun make-board (list-of-ships)
  (let* ((width (+ 10 (* 2 (length list-of-ships))))
	 (height (+ 10 (* 2 (length list-of-ships))))
	 (board (empty-board width height)))
    (loop for i in list-of-ships
	  for spaces = (try-position i width height)
	  do (loop for (x y) in spaces do (setf (nth x (nth y board)) i)))
    (make-instance 'board :spaces board :width width :height height)))

(defun new-game (&rest players)
  (let ((board (make-board (mapcan #'ships players))))
    (make-instance 'game
		   :moves (empty-board (width board) (height board))
		   :board board :players players :turn-stack players)))