(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; game creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-player (&rest ship-types)
  (let ((p (make-instance 'player)))
    (setf (ships p)
	  (mapcar (lambda (s) (make-instance s :player p)) ship-types))
    p))

(defun make-game (&rest players)
  (let ((board (make-board (mapcan #'ships players))))
    (make-instance 'game :board board :players players :waiting-for players :turn-stack players)))


;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((g game) (p player))
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:body (echo (board g) p)))))

;;;;;;;;;;;;;;;;;;;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod advance-turn ((g game))
  (if (cdr (turn-stack g))
      (pop (turn-stack g))
      (setf (turn-stack g) (players g))))

(defmethod fire ((g game) (p player) x y)
  (let ((result (make-instance 
		 (if (empty-space-at? (board g) x y) 'miss 'hit)
		 :player p :x x :y y)))
    (push result (history g))
    (setf (move (space-at (board g) x y)) result)
    result))