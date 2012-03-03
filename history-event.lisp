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

(defmethod emit-record ((m hit) (p player))
  (format nil "event: shot~%data: ~a~%~%event: turn~%data: ~a~%~%"
	  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) (text . ,(echo m p))))
	  (if (eq (player m) p) "Their Turn" "Your Turn")))

(defmethod emit-record ((m miss) (p player))
  (format nil "event: shot~%data: ~a~%~%event: turn~%data: ~a~%~%"
	  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) (text . ,(echo m p))))
	  (if (eq (player m) p) "Their Turn" "Your Turn")))