(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; game creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-player (&rest ship-types)
  (let ((p (make-instance 'player)))
    (setf (ships p)
	  (mapcar (lambda (s) (make-instance s :player p)) ship-types))
    p))

(defun make-game (&rest players)
  (let ((board (make-board (mapcan-f #'ships players))))
    (make-instance 'game :board board :players players :waiting-for players :turn-stack players)))

;;;;;;;;;;;;;;;;;;;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod turn-p ((g game) &optional (player (session-value :player))) 
  (eq (car (turn-stack g)) player))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((g game) (p player)) (echo (board g) p))

(defmethod emit-record ((g game) (p player))
  (apply #'concatenate 
	 (cons 'string 
	       (mapcar (lambda (r) (emit-record r p)) 
		       (reverse (history g))))))

(defmethod emit-record ((m hit) (p player))
  (format nil "event: shot~%data: ~a~%~%event: turn~%data: ~a~%~%"
	  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) (text . ,(echo m p))))
	  (if (eq (player m) p) "Their Turn" "Your Turn")))

(defmethod emit-record ((m miss) (p player))
  (format nil "event: shot~%data: ~a~%~%event: turn~%data: ~a~%~%"
	  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) (text . ,(echo m p))))
	  (if (eq (player m) p) "Their Turn" "Your Turn")))

;;;;;;;;;;;;;;;;;;;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod advance-turn ((g game))
  (if (cdr (turn-stack g))
      (pop (turn-stack g))
      (setf (turn-stack g) (players g))))

(defmethod fire ((g game) (p player) x y)
  (let* ((s (space-at (board g) x y)) 
	 (result (make-instance 
		  (if (empty-space? s) 'miss 'hit)
		  :player p :x x :y y)))
    (push result (history g))
    (unless (empty-space? s) (setf (ship result) (contents s)))
    (setf (move s) result)
    result))