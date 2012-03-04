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

;;;;;;;;;;;;;;;;;;;; history related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; creation
(defmethod to-json ((m move))
  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) 
			   (text . ,(echo m (session-value :player))))))

(defmethod push-record ((g game) event-type message)
  (push (make-instance 'history-event
		       :id (length (history g))
		       :event-type event-type
		       :message message)
	(history g)))

;;; display
(defmethod emit-record ((g game) (p player))
  (apply #'concatenate 
	 (cons 'string 
	       (mapcar (lambda (r) (emit-record r p)) 
		       (reverse (take 10 (history g)))))))

(defmethod emit-record ((e history-event) (p player))
  (format nil "id: ~a~%event: ~a~%data: ~a~%~%"
	  (id e) (event-type e) (message e)))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((g game) (p player)) (echo (board g) p))

(defmethod echo-console ((g game) (p player))
  (html-to-stout (:div :id "player-console" 
		       (:div :id "turn-marker" (str (if (turn-p *game*) "Your turn" "Their turn")))
		       (echo-stats p)
		       (:a :class "menu-item" :href "/quit-game" "Quit Game"))))

(defmethod echo-stats ((p player))
  (html-to-stout 
    (:div :class "player-ships"
	  (loop for s in (ships p)
		do (str (echo-stats s))))))

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
	(push-record g "ship-damage" (to-json ship))))
    (setf (move space) result)
    result))