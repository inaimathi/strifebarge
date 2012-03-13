(in-package :strifebarge)

(compile-js "js/strifebarge.js" "strifebarge-js.lisp"
	    (ps 
	      (define-event-source source "update-map")

	      (define-event-listener source "turn"
		(lambda (e) ($ "#turn-marker" (text (chain e data)))))

	      (define-event-listener source "ship-damage"
		(lambda (e)
		  (let* ((d (parse-json (chain e data))) 
			 (ship-id (+ "#" (@ d "shipId"))))
		    ($ ship-id (find ".num-hp") (text (@ d "hp")))
		    ($ ship-id (find ".hp-remaining") (width (+ (@ d "percent") "%"))))))

	      (define-event-listener source "ship-sunk"
		(lambda (e) 
		  (let ((d (parse-json (chain e data))))
		    ($ (+ "#opponent-ships ." (@ d "id")) 
		       (add-class "dead-ship")
		       (text (@ d "type"))))))
	      
	      (define-event-listener source "player-eliminated"
		(lambda (e) ($ (+ "#" (chain e data)) (add-class "dead-player"))))

	      (define-event-listener source "shot"
		(lambda (e) 
		  (let ((d (parse-json (chain e data))))
		    ($-space-at ((@ d "x") (@ d "y")) (html (@ d "text"))))))
	      
	      (defun send-shot (x y)
		(post-to "/turn" 
			 (create :x x :y y) 
			 (lambda (data)
			   ($-space-at (x y) (html data))
			   ($ "#turn-marker" (text "Their Turn")))))))