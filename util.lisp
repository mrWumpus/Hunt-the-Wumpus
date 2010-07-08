(in-package :net.sinawali.wumpus)

(defun range (max)
  (let ((ret-range ()))
    (dotimes (num max)
      (push num ret-range))
    ret-range))

(defun delete-random-item (lst)
  "Chose and remove an item from list. This is a destructive call"
  (let ((ret-item 
          (nth (random (length lst)) lst)))
    (delete ret-item lst)
    ret-item))
