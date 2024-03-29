(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun randval (n)
  (1+ (random (max 1 n))))

(defstruct monster (health (randval 10)))
(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))
(defmethod monster-attack (m)
  (princ "a monster is attacking you.. nothing happens..."))


(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))


(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn
	 (princ "You killed the ")
	 (princ (type-of m))
	 (princ "! "))
      (progn
	(princ "You hit the ")
	(princ (type-of m))
	(princ ", knocking of ")
	(princ x)
	(princ " health points! "))))




(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "*** DEAD ***")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

;; ============= MONSTERS ===================


;; ORC
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you an knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;; HYDrA
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated hydra falls on the floor!")
      (progn (princ "You lop off ")
	     (princ x )
	     (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;; SLIME

(defstruct (slime-mold (:include monster)) (slimeliness (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a slimeliness of ")
  (princ (slime-mold-slimeliness m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-slimeliness m))))
    (princ "A slime mold wraps around your legs and decreases your agility by")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point!")
      (decf *player-health*))))

;; BRIGAND
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points! "))
	  ((= x *player-strength*)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength*)))))

;; ============== MONSTERS END =============


(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number, amigo.")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))



(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun init-player ()
	     (setf *player-health* 30)
	     (setf *player-agility* 30)
	     (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab, [d]ouble swing, [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3))))))
	       (unless (monsters-dead)
		 (monster-hit (random-monster) 1)))))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed... GG"))
  (when (monsters-dead)
    (princ "Yesss you brutally slaughered all monsters!!! Hero!!!!")))
