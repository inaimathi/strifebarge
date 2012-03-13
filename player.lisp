(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; player creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-player (&rest ship-types)
  (let ((p (make-instance 'player)))
    (setf (ships p)
	  (mapcar (lambda (s) (make-instance s :player p)) ship-types))
    p))

;;;;;;;;;;;;;;;;;;;; predicates and getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod opponents ((g game) &optional (player (session-value :player)))
  (remove player (players g)))

(defmethod dead-p ((p player))
  (every #'dead-p (ships p)))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo-stats ((p player))
  (html-to-stout 
    (:div :class "player-ships"
	  (loop for s in (ships p)
		do (str (echo-stats s))))))

(defmethod echo-opponent ((p player))
  (html-to-stout
    (:h5 :id (instance-to-id p) 
	 :class (when (dead-p p) "dead-player") 
	 "An Opponent")
    (:ul (loop for s in (shuffle (ships p)) 
	       do (echo-opponent-ship s)))))