(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; board creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun empty-grid (width height)
  (loop for y from 0 to height
	collect (loop for x from 0 to width collect (make-space x y))))

(defun empty-board (width height)
  (make-instance 'board 
		 :spaces (empty-grid width height)
		 :width width
		 :height height))

;;;;;;;;;;;;;;;;;;;; board setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod space-at ((b board) x y) (nth x (nth y (spaces b))))

(defmethod assign-ship-spaces ((s ship) direction x y)
  (loop for i from 0 to (- (space-count s) 1)
	if (eq :vertical direction)
	  collect (cons x (+ i y))
	else
	  collect (cons (+ i x) y)))

(defmethod position-ship ((s ship) (b board))
  (let* ((x (random (- (width b) (space-count s))))
	 (y (random (- (height b) (space-count s))))
	 (direction (pick '(:vertical :horizontal)))
	 (ship-spaces (assign-ship-spaces s direction x y)))
    (if (every (lambda (p) (empty-space-at? b (car p) (cdr p))) ship-spaces)
	(progn (setf (direction s) direction)
	       (loop for (space-x . space-y) in ship-spaces
		     do (setf (contents (space-at b space-x space-y)) s)))
	(position-ship s b))))

(defun make-board (list-of-ships)
  (let* ((width (+ 5 (* 2 (length list-of-ships))))
	 (height (+ 5 (* 2 (length list-of-ships))))
	 (board (empty-board width height)))
    (dolist (s list-of-ships) (position-ship s board))
    board))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((b board) (p player))
  (with-html-output (*standard-output* nil :indent t)
    (:table :id "game-board"
	    (mapc (lambda (row) 
		    (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
		  (spaces b)))))