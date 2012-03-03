;;;; strifebarge.lisp

(in-package #:strifebarge)

(defparameter *game* nil)

;;;;;;;;;;;;;;;;;;;; full handlers
;;; (all of these either directly return, or redirect to complete pages)

(define-easy-handler (index :uri "/") ()
  (html-to-str
    (:html (:head (:title "StrifeBarge")
		  (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
	   (:body (:a :class "menu-item" :href "/new-game" "New Game")
		  (:a :class "menu-item" :href "/join-game" "Join Game")))))

(define-easy-handler (new-game :uri "/new-game") (player-count)
  (let* ((p-count (if player-count (parse-integer player-count) 2)) 
	 (players (loop repeat p-count collect (make-player 'carrier 'cruiser 'destroyer))))
    (setf *game* (apply #'make-game players))
    (redirect "/join-game")))

(define-easy-handler (join-game :uri "/join-game") ()
  (redirect-unless (and (not (null (waiting-for *game*)))
		    (null (session-value :player))))
  (setf (session-value :player) (pop (waiting-for *game*)))
  (redirect "/show-game"))

(define-easy-handler (show-game :uri "/show-game") ()
  (redirect-unless (not (null (session-value :player))))
  (html-to-str
    (:html (:head
	    (:title "StrifeBarge")
	    (:script :type "text/javascript" :src "/js/jquery-1.7.1.min.js")
	    (:script :type "text/javascript" :src "/js/strifebarge.js")
	    (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
	   (:body (echo-console *game* (session-value :player)) 
		  (echo *game* (session-value :player))))))

(define-easy-handler (quit-game :uri "/quit-game") ()
  (redirect-unless (not (null (session-value :player))))
  (push (waiting-for *game*) (session-value :player))
  (setf (session-value :player) nil)
  "You have quit the game")

;;;;;;;;;;;;;;;;;;;; ajax handlers
;;; these return either errors or partial json/html. 
;;; The caller is expected to transform their output before final display

(define-easy-handler (update-map :uri "/update-map") ()
  (assert (not (null (session-value :player))))
  (setf (header-out :cache-control) "no-cache"
	(content-type*) "text/event-stream")
  (emit-record *game* (session-value :player)))

(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (turn-p *game*) (stringp x) (stringp y)))
  (advance-turn *game*)
  (echo (fire *game* (session-value :player) (parse-integer x) (parse-integer y)) 
	(session-value :player)))