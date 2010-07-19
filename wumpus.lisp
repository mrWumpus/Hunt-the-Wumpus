(in-package :net.sinawali.wumpus)

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 2) (debug 3)))

(defvar *player-location* 0)
(defvar *arrow-count* 5)
(defvar *wumpus-location* 0) 
(defvar *pits* ())
(defvar *bats* ())
(defvar *game-over* nil)

(defparameter *map* *dodecahedron*)

(declaim (inline print-state))
(defun print-state ()
  (format t "Player location: ~D~%Wumpus room: ~D~%Pits: ~A~%Bats: ~A~%" *player-location* *wumpus-location* *pits* *bats*)
  (values))

(defun initialize()
  (let ((rooms (range 20)))
    (setf *player-location* (delete-random-item rooms))
    (setf *wumpus-location* (delete-random-item rooms))
    (setf *pits* ())
    (setf *bats* ())
    (push (delete-random-item rooms) *pits*) 
    (push (delete-random-item rooms) *bats*) 
    (push (delete-random-item rooms) *pits*) 
    (push (delete-random-item rooms) *bats*))
  (setf *arrow-count* 5)
  (setf *game-over* nil)
  (values))

(defun hunt-the-wumpus ()
  (initialize)
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
             (format *query-io* "I smell a Wumpus!~%"))
            ((is-pit-room cur-room)
             (format *query-io* "I feel a draft.~%"))
            ((is-bat-room cur-room)
             (format *query-io* "Bats nearby!~%"))))
    (format *query-io* "You are in room ~A~%Tunnels lead to ~A ~A ~A~%Command> "
            (room-number *player-location*)
            (room-number (first next-rooms))
            (room-number (first (rest next-rooms)))
            (room-number (first (last next-rooms))))))

(declaim (inline is-wumpus-room is-pit-room is-bat-room))

(defun is-wumpus-room (room)
  (= room *wumpus-location*))

(defun is-pit-room (room)
  (find room *pits*))

(defun is-bat-room (room)
  (find room *bats*))

(declaim (inline ask-instructions print-instructions))

(defun ask-instructions ()
  (when (y-or-n-p "Instructions") (print-instructions)))

(defun print-instructions ()
  (display-file "instructions.txt")
  (values))

(defun redo-loop (fun)
  (do ((doit t (y-or-n-p "Play again?")))
    ((null doit))
    (funcall fun)))

(defun move (to-room)
  (let ((next-rooms (nth *player-location* *map*)))
    (if (find to-room next-rooms)
        (progn
          (setf *player-location* to-room)
          t)
        nil)))
