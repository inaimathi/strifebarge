(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod to-json ((m move))
  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) 
			   (text . ,(echo m (session-value :player))))))

(defmethod push-record ((g game) event-type message)
  (push (make-instance 'history-event
		       :id (length (history g))
		       :event-type event-type
		       :message message)
	(history g)))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod emit-record ((g game) (p player))
  (apply #'concatenate 
	 (cons 'string 
	       (mapcar (lambda (r) (emit-record r p)) 
		       (reverse (take 10 (history g)))))))

(defmethod emit-record ((e history-event) (p player))
  (format nil "id: ~a~%event: ~a~%data: ~a~%~%"
	  (id e) (event-type e) (message e)))

;;;;;;;;;;;;;;;;;;;; game logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod death-check ((g game) (s ship))
  (when (dead-p s) 
    (push-record g "ship-sunk" 
		 (encode-json-to-string `((:type . ,(type-of s)) 
					  (:id . ,(instance-to-id s))))))
  (when (dead-p (player s))
    (push-record g "player-eliminated" (instance-to-id (player s)))))