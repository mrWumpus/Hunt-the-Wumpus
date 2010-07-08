(in-package :net.sinawali.wumpus)

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 2) (debug 3)))

(defvar *game-state* 0) ;;; 0 - all is normal, -1 - lose, +1 - win
(defvar *player-location* -1)
(defvar *current-player-location* -1)
(defvar *arrow-count* 5)
(defvar *wumpus-location* -1) 
(defvar *current-wumpus-location* -1) 
(defvar *pits* ())
(defvar *bats* ())

(declaim (inline print-state))
(defun print-state ()
  (format t "Player location: ~D~%Wumpus room: ~D~%Pits: ~A~%Bats: ~A~%" *current-player-location* *current-wumpus-location* *pits* *bats*)
  (values))

(defparameter *current-map* *dodecahedron*)

(defun initialize()
  (when (and (> *player-location* 0) (not (y-or-n-p "Same set-up?")))
          (let ((rooms (range 20)))
            (setf *player-location* (delete-random-item rooms))
            (setf *wumpus-location* (delete-random-item rooms))
            (setf *pits* ())
            (setf *bats* ())
            (push (delete-random-item rooms) *pits*) 
            (push (delete-random-item rooms) *bats*) 
            (push (delete-random-item rooms) *pits*) 
            (push (delete-random-item rooms) *bats*)))
  (reset))

(defun reset()
  (setf *current-player-location* *player-location*)
  (setf *current-wumpus-location* *wumpus-location*)
  (setf *arrow-count* 5)
  (setf *game-state* 0)
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
  (let ((next-rooms (nth *current-player-location* *current-map*)))
    (dolist (cur-room next-rooms)
      (cond ((is-wumpus-room cur-room)
             (format *query-io* "I smell a Wumpus!~%"))
            ((is-pit-room cur-room)
             (format *query-io* "I feel a draft.~%"))
            ((is-bat-room cur-room)
             (format *query-io* "Bats nearby!~%"))))
    (format *query-io* "You are in room ~A~%Tunnels lead to ~A ~A ~A"
            (room-number *current-player-location*)
            (room-number (first next-rooms))
            (room-number (first (rest next-rooms)))
            (room-number (first (last next-rooms)))))
  (values))

(declaim (inline is-wumpus-room is-pit-room is-bat-room))

(defun is-wumpus-room (room)
  (= room *current-wumpus-location*))

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

(defun move-player (room-number)
  (let ((to-room (read-room-number room-number))
        (next-rooms (nth *current-player-location* *current-map*)))
    (if (find to-room next-rooms)
      (progn
        (setf *current-player-location* to-room)
        (check-for-hazards))
      (format *query-io* "Invalid move.~%")))
  (values))

(defun move-wumpus ()
  (let ((to-room-idx (random 4)))
    (when (< to-room-idx 3)
      (setf *current-wumpus-location* 
                    (nth to-room-idx
                         (nth *current-wumpus-location* *current-map*)))))
  (when (eql *current-player-location* *current-wumpus-location*)
    (format *query-io* "Tsk tsk tsk - Wumpus got you!~%")
    (setf *game-state* -1)))

(defun check-for-hazards ()
  (when (is-wumpus-room *current-player-location*)
     (format *query-io* "...Oops! Bumped a Wumpus!~%")
     (move-wumpus))
  (when (eq 0 *game-state*)
    (cond
       ((is-pit-room *current-player-location*)
       (format *query-io* "YYYIIIEEE . . . Fell into a pit!~%")
        (setf *game-state* -1))
       ((is-bat-room *current-player-location*)
        (format *query-io* "ZAP--Super bat snatch! Elsewheresville for you!~%")
        (setf *current-player-location* (random 20))
        (check-for-hazards)))))
