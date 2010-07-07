(in-package :net.sinawali.wumpus)

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (safety 2) (debug 3)))

;;; Some maps
(defvar *dodecahedral-map*
  '((1 4 7) (0 2 9) (1 3 11) (2 4 13) (0 3 5)
	(4 6 14) (5 7 16) (0 6 8) (7 9 17) (1 8 10)
	(9 11 18) (2 10 12) (11 13 19) (3 12 14) (5 13 15)
	(14 16 19) (6 15 17) (8 16 18) (10 17 19) (12 15 18)))

(defvar *player-location* 0)
(defvar *arrow-count* 5)
(defvar *wumpus-location*) 
(defvar *pits*)
(defvar *bats*)

(defparameter *current-map* *dodecahedral-map*)

(defun hunt-the-wumpus ()
  (let ((*player-location* 0)
	    (*wumpus-location* (random 20))
	    (*pits* (list (random 20) (random 20)))
	    (*bats* (list (random 20) (random 20))))
	(format t "Hunt the Wumpus!~%")
	(print-location-with-warnings)))

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
