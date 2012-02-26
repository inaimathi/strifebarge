(in-package #:strifebarge)

(defun css-space-size ()
    (let ((d (format nil "~apx" *board-square-size*)))
      `(:width ,d :height ,d)))

(defmacro css-transform-origin (x y)
  (let ((d (format nil "~a ~a" x y)))
    `'(:transform-origin ,d
       :-ms-transform-origin ,d
       :-webkit-transform-origin ,d
       :-moz-transform-origin ,d
       :-o-transform-origin ,d)))

(defun css-rotate (degrees)
  (let ((d (format nil "rotate(~adeg)" degrees)))
    `(:transform ,d
      :-ms-transform ,d
      :-webkit-transform ,d
      :-o-transform ,d
      :-moz-transform ,d)))

(defun px (num) (format nil "~apx" num))

(defmethod css-left ((s ship))
  (px (if (eq :vertical (direction s))
	  (board-scale (+ (x s) (wid s)))
	  (board-scale (x s)))))

(compile-css "css/strifebarge.css"
	     `((body :background-color \#000 :background-image "url(/img/galaxy.png)" :padding 0px :margin 0px)
	       
	       (\#board-wrapper :position absolute)
	       (\#game-board :border-spacing 0px :color \#fff)
	       ("#game-board .miss" :font-family courier :font-size x-small)
	       ("#game-board .hit" ,@(css-space-size))
	       ("#game-board td" ,@(css-space-size) :padding 0px)
	       ("#game-board .shot-link" :height 100% :width 100% :display block)
	       ("#game-board .shot-link:hover" :background-position center :border none :background-image "url(/img/crosshairs/crosshair9.png)")

	       (\#player-console :float right :height 200px :width 100px :margin ,(px *board-square-size*) :padding 5px :background-color \#eee)
	       
	       (.ship ,@(css-transform-origin 0 0) :position absolute :z-index -10000)))