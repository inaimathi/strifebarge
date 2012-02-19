(in-package :strifebarge)

(defun class-p (class-name thing) (eq class-name (class-name (class-of thing))))

(defun pick (a-list) (nth (random (length a-list)) a-list))