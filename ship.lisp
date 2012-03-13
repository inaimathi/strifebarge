(in-package :strifebarge)


;;;;;;;;;;;;;;;;;;;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-point (direction x y i j)
  (if (eq :vertical direction)
      (list (+ i x) (+ j y))
      (list (+ j x) (+ i y))))

(defmethod assign-ship-spaces ((s ship) direction x y)
  "Given a ship, a direction and an initial x/y, 
returns a list of spaces that the ship will occupy."
  (mapcan (lambda (i) 
	    (mapcar (lambda (j)
		      (create-point direction x y i j)) 
		    (range 0 (- (len s) 1))))
	  (range 0 (- (wid s) 1))))

(defmethod position-ship ((s ship) (b board))
  "Given a ship and a board, positions a ship on the board,
ensuring there are no collisions."
  (let* ((direction (pick '(:vertical :horizontal)))
	 (ship-v-padding (+ 1 (if (eq :vertical direction) (len s) (wid s))))
	 (ship-h-padding (+ 1 (if (eq :vertical direction) (wid s) (len s)))) 
	 (x (random (- (width b) ship-h-padding))) 
	 (y (random (- (height b) ship-v-padding)))
	 (ship-spaces (assign-ship-spaces s direction x y)))
    (if (every (lambda (p) (empty-space-at? b (car p) (cadr p))) ship-spaces)
	(progn (setf (direction s) direction 
		     (x s) x 
		     (y s) y)
	       (loop for (space-x space-y) in ship-spaces
		     do (let ((current-space (space-at b space-x space-y))) 
			  (setf (contents current-space) s))))
	(position-ship s b))))

;;;;;;;;;;;;;;;;;;;; getters/predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod remaining-hp ((s ship))
  (- (space-count s) (damage s)))

(defmethod hp ((s ship)) (space-count s))

(defmethod hp-% ((s ship))
  (round (* 100 (/ (remaining-hp s) (space-count s)))))

(defmethod image-file ((s ship)) (format nil "/img/ships/~(~a~).png" (type-of s)))

(defmethod dead-p ((s ship))
  (zerop (- (space-count s) (damage s))))
;;;;;;;;;;;;;;;;;;;; echo methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo-stats ((s ship))
  (html-to-str
    (:div :id (instance-to-id s) :class "ship-stats" (:img :src (image-file s))
	  (htm (:div :class "total-hp" 
		     (:div :class "hp-remaining" 
			   :style (inline-css `(:width ,(format nil "~a%" (hp-% s))))
			   (:span :class "num-hp" (str (remaining-hp s))) "/" (:span :class "num-total-hp" (str (hp s)))))))))

(defmethod to-json ((s ship))
  (encode-json-to-string `((ship-id . ,(instance-to-id s)) (hp . ,(remaining-hp s)) (percent . ,(hp-% s)))))

(defmethod echo ((s ship) (p player))
  (let ((direction (direction s)))
    (html-to-str (:img :class "ship" 
		       :style (inline-css `(:left ,(css-left s) :top ,(px (board-scale (y s)))
					    :width ,(board-scale (len s)) :height ,(board-scale (wid s))
					    ,@(when (eq :vertical direction) (css-rotate 90))))
		       :src (image-file s)))))

(defmethod echo-opponent-ship ((s ship))
  (html-to-stout 
    (:li :class (format nil "~a ~@[~a~]" 
			(instance-to-id s) 
			(when (dead-p s) "dead-ship"))  
	 (str (if (dead-p s)
		  (string-downcase (type-of s))
		  "???")))))