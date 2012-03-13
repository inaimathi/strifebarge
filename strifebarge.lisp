;;;; strifebarge.lisp

(in-package #:strifebarge)

(defparameter *game* nil)

(defparameter *games-table* nil)

(defun get-open-games ()
  (loop for (name . game) in *games-table*
	when (waiting-for game)
	  collect name))

(defun get-game (game-name)
  (cdr (assoc game-name *games-table* :test #'string=)))

(defun test-display (&rest a-list) (format nil "~{~a~^ || ~}" a-list))
;;;;;;;;;;;;;;;;;;;; full handlers
;;; (all of these either directly return, or redirect to complete pages)

(define-easy-handler (index :uri "/") ()
  (html-to-str
    (:html (:head (:title "StrifeBarge")
		  (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
	   (:body (:a :class "menu-item" :href "/new-game" "New Game")
		  (:div :id "Games Menu"
			(loop for (name . g) in *games-table*
			      do (htm (:li (:a :href (format nil "/join-game?game-name=~a" name)
					       (str (format nil "~a[~a]" name (length (waiting-for g)))))))))
		  (:p (str (format nil "~{~a~}" (list (session-value :game)
						      (session-value :player)))))))))

(define-easy-handler (new-game :uri "/new-game") (player-count)
  (if (> 20 (length *games-table*))
      (let* ((p-count (if player-count (parse-integer player-count) 2)) 
	     (players (loop repeat p-count collect (make-player 'carrier 'cruiser 'destroyer)))
	     (game (apply #'make-game players))
	     (game-name (instance-to-id game)))
	(push `(,game-name . ,game) *games-table*)
	(redirect (format nil "/join-game?game-name=~a" game-name)))
      "No more room on this server"))

(define-easy-handler (join-game :uri "/join-game") (game-name)
  (let ((game (get-game game-name)))
    (redirect-unless (and game
			  (not (null (waiting-for game)))
			  (null (session-value :player))
			  (null (session-value :game))))
    (setf (session-value :player) (pop (waiting-for game))
	  (session-value :game) game)
    (redirect "/show-game")))

(define-easy-handler (show-game :uri "/show-game") ()
  (redirect-unless (not (null (session-value :player))))
  (html-to-str
    (:html (:head
	    (:title "StrifeBarge")
	    (:script :type "text/javascript" :src "/js/jquery-1.7.1.min.js")
	    (:script :type "text/javascript" :src "/js/strifebarge.js")
	    (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
	   (:body (echo-console (session-value :game) (session-value :player)) 
		  (echo (session-value :game) (session-value :player))))))

(define-easy-handler (quit-game :uri "/quit-game") ()
  (redirect-unless (not (null (session-value :player))))
  (push (session-value :player) (waiting-for (session-value :game)))
  (setf (session-value :player) nil (session-value :game) nil)
  "You have quit the game")

;;;;;;;;;;;;;;;;;;;; ajax handlers
;;; these return either errors or partial json/html. 
;;; The caller is expected to transform their output before final display

(define-easy-handler (update-map :uri "/update-map") ()
  (assert (not (null (session-value :player))))
  (setf (header-out :cache-control) "no-cache"
	(content-type*) "text/event-stream")
  (emit-record (session-value :game) (session-value :player)))

(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (turn-p (session-value :game)) (stringp x) (stringp y)))
  (advance-turn (session-value :game))
  (echo (fire (session-value :game) (session-value :player) (parse-integer x) (parse-integer y)) 
	(session-value :player)))