(in-package :strifebarge)

(defmethod empty-space? ((s board-space)) (null (contents s)))
(defmethod empty-space-at? ((b board) x y) (null (contents (space-at b x y))))

(defmethod echo ((s board-space) (p player))
  (with-html-output (*standard-output* nil :indent t)
    (:td (cond ((eq (move s) :hit) (str "X"))
	       ((move s) (str "O"))
	       ((and (contents s) (eq (player (contents s)) p)) (str "#"))
	       (t (htm (:a :href (format nil "/turn?x=~a&y=~a" (x s) (y s)) "~")))))))