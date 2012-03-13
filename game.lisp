(in-package :strifebarge)

(defparameter *games-table* nil)

;;;;;;;;;;;;;;;;;;;; game creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-game (&rest players)
  (let ((board (make-board (mapcan-f #'ships players))))
    (make-instance 'game :board board :players players :waiting-for players :turn-stack players)))

;;;;;;;;;;;;;;;;;;;; predicates and getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-game (game-name)
  (cdr (assoc game-name *games-table* :test #'string=)))

(defmethod ships ((g game)) 
  (mapcan-f #'ships (players g)))

(defmethod turn-p ((g game) &optional (player (session-value :player))) 
  (eq (car (turn-stack g)) player))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((g game) (p player)) (echo (board g) p))

(defmethod echo-console ((g game) (p player))
  (html-to-stout 
    (:div :id "player-console" 
	  (:div :id "turn-marker" (str (if (turn-p g p) "Your turn" "Their turn")))
	  (echo-stats p)
	  (:div :id "opponent-ships"
		(:h3 "Game Name")
		(loop for a-player in (opponents g p)
		      do (echo-opponent a-player)))
	  (:a :class "menu-item" :href "/quit-game" "Quit Game"))))

;;;;;;;;;;;;;;;;;;;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod advance-turn ((g game))
  (if (cdr (turn-stack g))
      (pop (turn-stack g))
      (setf (turn-stack g) (players g))))

(defmethod fire ((g game) (p player) x y)
  (let* ((space (space-at (board g) x y))
	 (result (make-instance 
		  (if (empty-space? space) 'miss 'hit)
		  :player p :x x :y y)))
    (push-record g "shot" (to-json result))
    (unless (empty-space? space)
      (let ((ship (contents space)))
	(setf (ship result) ship)
	(incf (damage ship))
	(push-record g "ship-damage" (to-json ship))
	(death-check g ship)))
    (setf (move space) result)
    result))