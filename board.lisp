(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; board creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun empty-grid (width height)
  (loop for x from 0 to height
	collect (loop for y from 0 to width collect *empty-space*)))

(defun empty-board (width height)
  "Returns a collection of [height] rows, each of length [width].
Each cell is independant (which is why the board isn't built with make-list)"
  (make-instance 'board 
		 :spaces (empty-grid width height)
		 :moves (empty-grid width height)
		 :width width
		 :height height))

;;;;;;;;;;;;;;;;;;;; board setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod empty-space? ((b board) space)
  (eq *empty-space* (pt b (car space) (cdr space))))

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
    (if (every (lambda (p) (empty-space? b p)) ship-spaces)
	(progn 
	  (setf (coords s) ship-spaces
		(direction s) direction)
	  (loop for (x . y) in ship-spaces
		do (setf (nth x (nth y (spaces b))) s)))
	(position-ship s b))))

(defun make-board (list-of-ships)
  "Returns a board object. 
A board is both a ship-placement map and a moves-map.
The first tracks ship positions, the second tracks what shots have been taken already."
  (let* ((width (+ 10 (* 2 (length list-of-ships))))
	 (height (+ 10 (* 2 (length list-of-ships))))
	 (board (empty-board width height)))
    (dolist (s list-of-ships) (position-ship s board))
    board))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; technically a player method, but only used to echo a board
(defmethod combine-space ((p player) (s ship) move-space)
  (cond ((not (eq move-space *empty-space*)) move-space)
	((eq (player s) p) s)
	(t *empty-space*)))

(defmethod combine-space ((p player) map-space move-space)
  (if (not (eq move-space *empty-space*))
      move-space
      *empty-space*))
  ;;;;;;;;;;;;;;;;;;;

(defmethod echo ((b board) (p player))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:table
     (loop for map-row in (spaces b)
	   for move-row in (moves b)
	   do (htm 
	       (:tr (loop for map-space in map-row
			  for move-space in move-row
			  do (htm (:td (str (combine-space p map-space move-space)))))))))))

;;;;;;;;;;;;;;;;;;;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pt ((b board) x y) (nth x (nth y (spaces b))))

(defmethod fire ((b board) x y)
  (let ((result  (if (eq *empty-space* (pt g x y)) :miss :hit)))
    (setf (nth x (nth y (moves b))) result)
    result))