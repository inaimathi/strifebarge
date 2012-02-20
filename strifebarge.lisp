;;;; strifebarge.lisp
(in-package #:strifebarge)

(define-easy-handler (index :uri "/") ()
  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
		       (make-player 'carrier 'cruiser 'destroyer))))
    (echo (apply #'new-game players) (car players))))