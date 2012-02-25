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

(defmacro if-vertical (do-if do-unless)
  `(if (eq :vertical direction) ,do-if ,do-unless))

(defun create-point (direction x y i j)
  (if (eq :vertical direction)
      (list (+ i x) (+ j y) i j)
      (list (+ j x) (+ i y) i j)))

(defmethod assign-ship-spaces ((s ship) direction x y)
  "Given a ship, a direction and an initial x/y, 
returns a list of spaces that the ship will occupy."
  (mapcan (lambda (i) 
	    (mapcar (lambda (j)
		      (create-point direction x y i j)) 
		    (range 0 (- (len s) 1))))
	  (range 0 (- (wid s) 1))))

(defmethod position-ship ((s ship) (b board))
  "Given a ship and a board, positions a ship on the board,
ensuring there are no collisions."
  (let* ((direction (pick '(:vertical :horizontal)))
	 (ship-v-padding (+ 1 (if (eq :vertical direction) (len s) (wid s))))
	 (ship-h-padding (+ 1 (if (eq :vertical direction) (wid s) (len s)))) 
	 (x (random (- (width b) ship-h-padding))) 
	 (y (random (- (height b) ship-v-padding)))
	 (ship-spaces (assign-ship-spaces s direction x y)))
    (if (every (lambda (p) (empty-space-at? b (car p) (cadr p))) ship-spaces)
	(progn (setf (direction s) direction)
	       (loop for (space-x space-y sprite-x sprite-y) in ship-spaces
		     do (let ((current-space (space-at b space-x space-y))) 
			  (setf (contents current-space) s
				(sprite-x current-space) sprite-x
				(sprite-y current-space) sprite-y))))
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
  (html-to-stout
    (:table :id "game-board"
	    (mapc (lambda (row) 
		    (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
		  (spaces b)))))