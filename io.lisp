(in-package :net.sinawali.wumpus)

(defun display-file (file)
  "Simply display the contents of the file to *query-io*. Avoids
  mulitple lines of formats or prints"
  (with-open-file (fstream file)
    (do ((line (read-line fstream nil) (read-line fstream nil)))
      ((null line))
      (format *query-io* "~A~%" line))))
