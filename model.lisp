(in-package :strifebarge)

(defclass ship ()
  ((space-count :reader space-count :initarg :space-count)
   (player :reader player :initarg :player)
   (damage :accessor damage :initform 0)
   (direction :accessor direction :initarg :direction)))

(defclass carrier (ship) ((space-count :initform 5)))
(defclass cruiser (ship) ((space-count :initform 3)))
(defclass destroyer (ship) ((space-count :initform 2)))

(defclass move ()
  ((player :reader player :initarg :player)
   (x :reader x :initarg :x)
   (y :reader y :initarg :y)))

(defclass hit (move) ())
(defclass miss (move) ())

(defclass player ()
  ((score :accessor score :initform 0)
   (sunken :accessor sunken :initarg :sunken)
   (ships :accessor ships :initarg :ships)))

(defclass board-space ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (contents :accessor contents :initform nil)
   (move :accessor move :initform nil)))

(defclass board ()
  ((width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (spaces :accessor spaces :initarg :spaces)))

(defclass game ()
  ((board :accessor board :initarg :board)
   (players :accessor players :initarg :players)
   (waiting-for :accessor waiting-for :initarg :waiting-for)
   (turn-stack :accessor turn-stack :initarg :turn-stack)
   (history :accessor history :initform nil)))