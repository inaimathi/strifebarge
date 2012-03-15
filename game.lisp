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

(defmethod remaining-players ((g game))
  (remove-if #'dead-p (players g)))

(defmethod current-player ((g game))
  (car (turn-stack g)))

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
      (setf (turn-stack g) (players g)))
  (setf (turn-started g) (now))
  (incf (turn-count g)))

(defmethod kick ((g game) (p player))
  (setf (players g) (remove p (players g))))

(defmethod victory-p ((g game))
  "Returns nil if a victory is undecided, otherwise
returns the winning player."
  (let ((players (remaining-players g)))
    (when (= 1 (length players)) (car players))))

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

;;;;;;;;;;;;;;;;;;;; ongoing actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defactor ticker ((game-list *games-table*)) (m)
  (sleep *game-ticker-frequency*)
  (loop for (game-name . g) in game-list
	do (update-state g))
  (send self nil)
  next)

(defvar *ticker* (ticker))
(send *ticker* nil)

(defmethod update-state ((g game))
  (when (>= (turns-missed (current-player g)) (turns-missed-allowed g)) 
    (kick g (current-player g)))
  (when (duration> (time-difference (turn-started g) (now)) (turn-time-limit g))
    (incf (turns-missed (current-player g)))
    (advance-turn g)))