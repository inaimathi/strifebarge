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
    (:div :id "board-wrapper"
	  (loop for s in (ships p)
		do (str (echo s p)))
	  (:table :id "game-board"
		  (mapc (lambda (row) 
			  (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
			(spaces b))))))