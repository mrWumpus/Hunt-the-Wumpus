(in-package :net.sinawali.wumpus)

(declaim (optimize (speed 1) (space 0) (compilation-speed 0) (safety 2) (debug 3)))

(defclass wumpus-game ()
  ((status :accessor status :initarg :status :initform nil)
   (player-location :accessor player-location :initarg :player-location :initform nil)
   (wumpus-location :accessor wumpus-location :initarg :wumpus-location :initform nil)
   (arrow-count :accessor arrow-count :initarg :arrow-count :initform nil)
   (game-map :accessor game-map :initarg :game-map :initform nil)
   (pits :accessor pits :initarg :pits :initform nil)
   (bats :accessor bats :initarg :bats :initform nil)))

(defun copy-game (game)
  (declare (wumpus-game game))
  (let ((copy (make-instance 'wumpus-game
                     :status (status game) 
                     :arrow-count (status game) 
                     :game-map (game-map game)
                     :player-location (player-location game)
                     :wumpus-location (wumpus-location game)
                     :pits (pits game)
                     :bats (bats game))))
    copy))

(declaim (inline next-rooms next-player-rooms next-wumpus-rooms 
                 is-wumpus-room is-pit-room is-bat-room
                 in-wumpus-room in-pit-room in-bat-room))

(defun next-rooms (game location)
  (declare (wumpus-game game))
  (nth location (game-map game)))

(defun next-player-rooms (game)
  (declare (wumpus-game game))
  (next-rooms game (player-location game)))

(defun next-wumpus-rooms (game)
  (declare (wumpus-game game))
  (next-rooms game (wumpus-location game)))

(defun is-wumpus-room (game room)
  (declare (wumpus-game game))
  (= room (wumpus-location game)))

(defun is-pit-room (game room)
  (declare (wumpus-game game))
  (find room (pits game)))

(defun is-bat-room (game room)
  (declare (wumpus-game game))
  (find room (bats game)))

(defun in-wumpus-room (game)
  (declare (wumpus-game game))
  (is-wumpus-room game (player-location game)))

(defun in-pit-room (game)
  (declare (wumpus-game game))
  (is-pit-room game (player-location game)))

(defun in-bat-room (game)
  (declare (wumpus-game game))
  (is-bat-room game (player-location game)))

(defun get-map (idx)
  (eval (cadr (elt *maps* idx))))

(defun select-map ()
  (dotimes (idx (length *maps*))
    (let ((cur-map (elt *maps* idx)))
      (format *query-io* "~D: ~A~%" (1+ idx) (car cur-map))))
  (let ((map-num (input-value "Please select map:"
                              :validator (lambda (val)
                                           (and (numberp val) (> val 0) (<= val (length *maps*)))))))
    (get-map (1- map-num))))

(defun move-player! (game room-number)
  (declare (wumpus-game game))
  (let ((to-room (read-room-number room-number))
        (next-rooms (next-player-rooms game)))
    (if (find to-room next-rooms)
      (progn
        (setf (player-location game) to-room)
        (check-for-hazards! game))
      (format *query-io* "Invalid move.~%")))
  game)

(defun move-wumpus! (game)
  (declare (wumpus-game game))
  (let ((to-room-idx (random 4)))
    (when (< to-room-idx 3)
      (setf (wumpus-location game)
                    (nth to-room-idx
                         (next-wumpus-rooms game)))))
  (when (in-wumpus-room game)
    (format *query-io* "Tsk tsk tsk - Wumpus got you!~%")
    (setf (status game) -1))
  game)

(defun check-for-hazards! (game)
  (declare (wumpus-game game))
  (when (in-wumpus-room game)
     (format *query-io* "...Oops! Bumped a Wumpus!~%")
     (move-wumpus! game))
  (when (eq 0 (status game))
    (cond
       ((in-pit-room game)
       (format *query-io* "YYYIIIEEE . . . Fell into a pit!~%")
        (setf (status game) -1))
       ((in-bat-room game)
        (format *query-io* "ZAP--Super bat snatch! Elsewheresville for you!~%")
        (setf (player-location game) (random 20))
        (check-for-hazards! game))))
  game)

(defun print-location-with-warnings (game)
  (declare (wumpus-game game))
  (let ((next-rooms (next-player-rooms game)))
    (dolist (cur-room next-rooms)
      (cond ((is-wumpus-room game cur-room)
             (format *query-io* "I smell a Wumpus!~%"))
            ((is-pit-room game cur-room)
             (format *query-io* "I feel a draft.~%"))
            ((is-bat-room game cur-room)
             (format *query-io* "Bats nearby!~%"))))
    (format *query-io* "You are in room ~A with ~D arrow~:P.~%Tunnels lead to ~A ~A ~A~%~%"
            (room-number (player-location game))
            (arrow-count game)
            (room-number (first next-rooms))
            (room-number (first (rest next-rooms)))
            (room-number (first (last next-rooms))))))

(defvar *game-setup* nil)
(defvar *current-game*)

(defun print-state (game)
  (declare (wumpus-game game))
  (format t "Player location: ~D~%Wumpus room: ~D~%Pits: ~A~%Bats: ~A~%Status: ~A~%"
          (player-location game)
          (wumpus-location game)
          (pits game)
          (bats game)
          (status game))
  (values))

(defun initialize()
  (when (or (null *game-setup*) (not (y-or-n-p "Same set-up?")))
          (let* ((rooms (range 20))
                 (player-loc (delete-random-item rooms))
                 (wump-loc (delete-random-item rooms))
                 (current-map (select-map))
                 (new-game (make-instance 'wumpus-game
                             :game-map current-map
                             :player-location player-loc
                             :wumpus-location wump-loc
                             :pits ()
                             :bats ())))
            (push (delete-random-item rooms) (pits new-game)) 
            (push (delete-random-item rooms) (bats new-game)) 
            (push (delete-random-item rooms) (pits new-game)) 
            (push (delete-random-item rooms) (bats new-game))
            (setf *game-setup* new-game)))
  (reset))

(defun reset()
  (let ((game-copy (copy-game *game-setup*)))
    (setf (status game-copy) 0)
    (setf (arrow-count game-copy) 5)
    (setf *current-game* game-copy)))

(defun move-or-shoot (wumpus-game)
  (declare (wumpus-game wumpus-game))
  (let ((cmd
          (input-string "(m)ove or (s)hoot?"
                        :validator (lambda (str)
                                     (let ((lc-str (string-downcase str)))
                                       (or (string= lc-str "m")
                                           (string= lc-str "s"))))
                        :error-prompt "Please enter 'm' or 's'.")))
    (cond
      ((string= (string-downcase cmd) "m")
       (move-input wumpus-game))
      ((string= (string-downcase cmd) "s")
       (shoot-input wumpus-game)))))

(defun move-input (wumpus-game)
  (declare (wumpus-game wumpus-game))
  (let ((to-room (input-value "Which room?"
                              :validator (lambda (val)
                                           (and (integerp val) (> val 0) (< val 21)))
                              :error-prompt "Please enter a valid room number.")))
    (move-player! wumpus-game to-room)))

(defun shoot-input (wumpus-game)
  (declare (wumpus-game wumpus-game))
  (decf (arrow-count wumpus-game))
  (let* ((num-rooms (input-value "Number of rooms (1-5)"
                                 :validator (lambda (val)
                                              (and (integerp val) (> val 0) (< val 6)))
                                 :error-prompt "Please enter a number between 1 and 5"))
         (room-list (collect-shoot-rooms num-rooms))
         (cur-room (player-location wumpus-game)))
    (dolist (next-room room-list)
      (let ((next-rooms (next-rooms wumpus-game cur-room)))
        (if (find next-room next-rooms)
          (setf cur-room next-room)
          (setf cur-room (nth (random (length next-rooms)) next-rooms)))
        (cond
          ((is-wumpus-room wumpus-game cur-room)
           (setf (status wumpus-game) 1)
           (format *query-io* "AHA! You got the Wumpus!~%")
           (return-from shoot-input))
          ((eq (player-location wumpus-game) cur-room)
           (setf (status wumpus-game) -1)
           (format *query-io* "Ouch! Arrow got you!~%")
           (return-from shoot-input)))))
    (when (eq 0 (arrow-count wumpus-game))
      (setf (status wumpus-game) -1))
    (format *query-io* "Missed!~%")
    (move-wumpus! wumpus-game)))

(defun collect-shoot-rooms (num)
  (let ((retlist nil))
    (dotimes (x num)
      (push (input-value "Room#"
                         :validator (lambda (val)
                                      (and (integerp val) (> val 0) (< val 21)))
                         :error-prompt "Please enter a valid room number."
                         :transformer (lambda (val)
                                        (read-room-number val)))
            retlist))
    (nreverse retlist)))

(declaim (inline room-number read-room-number))
;;; Display the room numbers starting at 0.
(defun room-number (room)
  (1+ room))

(defun read-room-number (room)
  (1- room))

(declaim (inline ask-instructions print-instructions))

(defun ask-instructions ()
  (when (y-or-n-p "Instructions") (print-instructions)))

(defun print-instructions ()
  (display-file "instructions.txt"))

(defun redo-loop (fun)
  (do ((doit t (y-or-n-p "Play again?")))
    ((null doit))
    (funcall fun)))

(defun epilogue (wumpus-game)
  (declare (wumpus-game) (wumpus-game))
  (with-slots
    (status) wumpus-game
    (if
      (< status 0)
      (format *query-io* "Ha ha ha, you lose!~%")
      (format *query-io* "Hee hee hee, the Wumpus'll getcha next time.~%"))))

(defun hunt-the-wumpus ()
  (let ((*game-setup* nil))
    (format *query-io* "Hunt the Wumpus!~%")
    (ask-instructions)
    (redo-loop (lambda ()
        (let ((game (initialize)))
          (do ()
            ((not (eq 0 (status game))))
            (print-location-with-warnings game)
            (move-or-shoot game))
          (epilogue game))))
    (values)))
