;;;; strifebarge.lisp
(in-package #:strifebarge)

(defparameter *game* nil)

(define-easy-handler (index :uri "/") ()
  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
		       (make-player 'carrier 'cruiser 'destroyer))))
    (echo (apply #'make-game players) (car players))))

(define-easy-handler (new-game :uri "/new-game") ()
  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
		       (make-player 'carrier 'cruiser 'destroyer)
		       (make-player 'carrier 'cruiser 'destroyer))))
    (setf *game* (apply #'make-game players))
    (redirect "/show-game")))

(define-easy-handler (show-game :uri "/show-game") ()
  (echo *game* (car (players *game*))))

(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (stringp x) (stringp y)))
  (fire *game* (car (players *game*)) (parse-integer x) (parse-integer y))
  (redirect "/show-game"))