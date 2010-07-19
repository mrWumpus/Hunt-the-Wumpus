(in-package :net.sinawali.wumpus)

(defun display-file (file)
  "Simply display the contents of the file to *query-io*. Avoids
  mulitple lines of lisp code formats or prints"
  (with-open-file (fstream file)
    (do ((line (read-line fstream nil) (read-line fstream nil)))
      ((null line))
      (format *query-io* "~A~%" line))))

(defun user-input (message)
  (format *query-io* "~A" message)
  (force-output *query-io*)
  (read-line *query-io*))
