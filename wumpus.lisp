(in-package :net.sinawali.wumpus)

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 2) (debug 3)))

(defvar *player-location* 0)
(defvar *arrow-count* 5)
(defvar *wumpus-location* 0) 
(defvar *pits* ())
(defvar *bats* ())

(defparameter *current-map* *dodecahedron*)

(defun setup-positions()
  (let ((rooms (range 20)))
    (setf *player-location* (random-item rooms))
    (setf *wumpus-location* (random-item rooms))
    (setf *pits* ())
    (setf *bats* ())
    (push (random-item rooms) *pits*) 
    (push (random-item rooms) *bats*) 
    (push (random-item rooms) *pits*) 
    (push (random-item rooms) *bats*))) 

(defun hunt-the-wumpus ()
  (setup-positions)
  (format *query-io* "Hunt the Wumpus!~%")
  (print-location-with-warnings))

(declaim (inline room-number read-room-number))
;;; Display the room numbers starting at 0.
(defun room-number (room)
  (1+ room))

(defun read-room-number (room)
  (1- room))

(defun print-location-with-warnings ()
  (let ((next-rooms (nth *player-location* *current-map*)))
    (dolist (cur-room next-rooms)
      (cond ((is-wumpus-room cur-room)
             (format t "I smell a Wumpus!~%"))
            ((is-pit-room cur-room)
             (format t "I feel a draft.~%"))
            ((is-bat-room cur-room)
             (format t "Bats nearby!~%"))))
    (format t "You are in room ~A~%Tunnels lead to ~A ~A ~A~%Command> "
            (room-number *player-location*)
            (room-number (first next-rooms))
            (room-number (first (rest next-rooms)))
            (room-number (first (last next-rooms))))))

(declaim (inline is-wumpus-room is-pit-room is-bat-room is-room-in-list))

(defun is-wumpus-room (room)
  (= room *wumpus-location*))

(defun is-pit-room (room)
  (is-room-in-list room *pits*))

(defun is-bat-room (room)
  (is-room-in-list room *bats*))

(defun is-room-in-list (cur-room list)
  (find cur-room list))

(defun ask-instructions ()
  (format *query-io* "Instructions ")
  (force-output *query-io*)
  (when (y-or-n-p) (print-instructions)))

(defun print-instructions ()
  (display-file "instructions.txt")
  (values))

(defun redo-loop (fun)
  (do ((doit t (y-or-n-p "Play again?")))
    ((null doit))
    (funcall fun)))
