(in-package :strifebarge)

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun range (a b)
  "Returns a list of numbers starting with a and ending with b inclusive."
  (loop for i from a to b collect i))