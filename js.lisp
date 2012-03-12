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
		(lambda (e) (update-ship-list (parse-json (chain e data)))))

	      (define-event-listener source "shot"
		(lambda (e) 
		  (let ((d (parse-json (chain e data))))
		    ($-space-at ((@ d "x") (@ d "y")) (html (@ d "text"))))))

	      (defun update-ship-list (data)
		($ "#ship-list" (html (game-table data))))

	      (defun game-table (game-json)
		(+ (who-ps-html (:h3 (@ game-json :name)))
		   (chain (loop for p in (@ game-json :players)
				collect (who-ps-html (:h5 (@ p :name))
						     (:ul (ship-list (@ p :ships)))))
			  (join ""))))

	      (defun ship-list (ships)
		(let ((acc ""))
		  (chain $ (each ships 
				 (lambda (k v) 
				   (let ((name (if (= v "false") 
						   (who-ps-html (:li k))
						   (who-ps-html (:li :style "text-decoration: line-through;" k))))))
				   (setf acc (+ acc name)))))
		  acc))
	      
	      (defun send-shot (x y)
		(post-to "/turn" 
			 (create :x x :y y) 
			 (lambda (data)
			   ($-space-at (x y) (html data))
			   ($ "#turn-marker" (text "Their Turn")))))))