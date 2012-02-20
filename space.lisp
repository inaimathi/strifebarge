(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-space (x y) 
  (make-instance 'board-space :x x :y y))

;;;;;;;;;;;;;;;;;;;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod empty-space? ((s board-space)) (null (contents s)))
(defmethod empty-space-at? ((b board) x y) (null (contents (space-at b x y))))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((s board-space) (p player))
  (with-html-output (*standard-output* nil :indent t)
    (:td (cond ((move s) (echo (move s) p))
	       ((and (contents s) (eq (player (contents s)) p)) (str "#"))
	       (t (htm (:a :href (format nil "/turn?x=~a&y=~a" (x s) (y s)) "~")))))))

(defmethod echo ((m hit) (p player))
  (with-html-output (*standard-output* nil :indent t)
    "X"))

(defmethod echo ((m miss) (p player))
  (with-html-output (*standard-output* nil :indent t)
    "O"))