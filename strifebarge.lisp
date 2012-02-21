;;;; strifebarge.lisp

(in-package #:strifebarge)

(defparameter *game* nil)

(define-easy-handler (index :uri "/") ()
  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
		       (make-player 'carrier 'cruiser 'destroyer))))
    (echo (apply #'make-game players) (car players))))

(define-easy-handler (new-game :uri "/new-game") (player-count)
  (let* ((p-count (if player-count (parse-integer player-count) 2)) 
	 (players (loop for i from 1 to p-count
			collect (make-player 'carrier 'cruiser 'destroyer))))
    (setf *game* (apply #'make-game players))
    (redirect "/join-game")))

(define-easy-handler (join-game :uri "/join-game") ()
  (assert (and (not (null (waiting-for *game*)))
	       (null (session-value :player))))
  (setf (session-value :player) (pop (waiting-for *game*)))
  (redirect "/show-game"))

(define-easy-handler (show-game :uri "/show-game") ()
  (assert (not (null (session-value :player))))
  (echo *game* (session-value :player)))

(define-easy-handler (quit-game :uri "/quit-game") ()
  (assert (not (null (session-value :player))))
  (push (waiting-for *game*) (session-value :player))
  (setf (session-value :player) nil)
  "You have quit the game")

(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (eq (car (turn-stack *game*)) (session-value :player))
	       (stringp x) (stringp y)))
  (advance-turn *game*)
  (fire *game* (session-value :player) (parse-integer x) (parse-integer y))
  (redirect "/show-game"))