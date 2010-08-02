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

(defmethod copy-game ((game wumpus-game))
  (let ((copy (make-instance 'wumpus-game
                     :status (status game) 
                     :arrow-count (status game) 
                     :game-map (game-map game)
                     :player-location (player-location game)
                     :wumpus-location (wumpus-location game)
                     :pits (pits game)
                     :bats (bats game))))
    copy))


(defmethod next-player-rooms ((game wumpus-game))
  (nth (player-location game) (game-map game)))

(defmethod next-wumpus-rooms ((game wumpus-game))
  (nth (wumpus-location game) (game-map game)))

(defmethod is-wumpus-room ((game wumpus-game) room)
  (= room (wumpus-location game)))

(defmethod is-pit-room ((game wumpus-game) room)
  (find room (pits game)))

(defmethod is-bat-room ((game wumpus-game) room)
  (find room (bats game)))

(defmethod in-wumpus-room ((game wumpus-game))
  (is-wumpus-room game (player-location game)))

(defmethod in-pit-room ((game wumpus-game))
  (is-pit-room game (player-location game)))

(defmethod in-bat-room ((game wumpus-game))
  (is-bat-room game (player-location game)))

(defmethod move-player! ((game wumpus-game) room-number)
  (let ((to-room (read-room-number room-number))
        (next-rooms (next-player-rooms game)))
    (if (find to-room next-rooms)
      (progn
        (setf (player-location game) to-room)
        (check-for-hazards! game))
      (format *query-io* "Invalid move.~%")))
  game)

(defmethod move-wumpus! ((game wumpus-game))
  (let ((to-room-idx (random 4)))
    (when (< to-room-idx 3)
      (setf (wumpus-location game)
                    (nth to-room-idx
                         (next-wumpus-rooms game)))))
  (when (in-wumpus-room game)
    (format *query-io* "Tsk tsk tsk - Wumpus got you!~%")
    (setf (status game) -1))
  game)

(defmethod check-for-hazards! ((game wumpus-game))
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

(defmethod print-location-with-warnings ((game wumpus-game))
  (let ((next-rooms (next-player-rooms game)))
    (dolist (cur-room next-rooms)
      (cond ((is-wumpus-room game cur-room)
             (format *query-io* "I smell a Wumpus!~%"))
            ((is-pit-room game cur-room)
             (format *query-io* "I feel a draft.~%"))
            ((is-bat-room game cur-room)
             (format *query-io* "Bats nearby!~%"))))
    (format *query-io* "You are in room ~A~%Tunnels lead to ~A ~A ~A~%"
            (room-number (player-location game))
            (room-number (first next-rooms))
            (room-number (first (rest next-rooms)))
            (room-number (first (last next-rooms)))))
  (values))

(defvar *game-setup* nil)
(defvar *current-game*)

(defun print-state (game)
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
                 (new-game (make-instance 'wumpus-game
                             :game-map *dodecahedron*
                             :player-location (delete-random-item rooms)
                             :wumpus-location (delete-random-item rooms)
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
  (format *query-io* "SHOOT!~%"))

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

(defun hunt-the-wumpus ()
  (let ((*game-setup* nil))
    (format *query-io* "Hunt the Wumpus!~%")
    (ask-instructions)
    (redo-loop (lambda ()
        (do ((game (initialize)))
          ((not (eq 0 (status game))))
          (print-location-with-warnings game)
          (move-or-shoot game))))
    (values)))

