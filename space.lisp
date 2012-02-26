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
  (html-to-stout 
    (:td (cond ((move s) (str (echo (move s) p)))
	       ((and (contents s) (eq (player (contents s)) p)) (str ""))
	       (t (htm (:a :href "#" :class "shot-link" :onclick (format nil "sendShot(~a, ~a);" (x s) (y s)) "")))))))

(defmethod echo ((m hit) (p player)) 
  (html-to-str (:img :class "hit" :src "/img/explosion.png")))

(defmethod echo ((m miss) (p player))
  (html-to-str (:span :class "miss" "miss")))