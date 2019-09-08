(defparameter *nodes* '((living-room (You are in the living room.
				      A beautiful woman is painting on the dining table.))
			(hallway (You are in the hallway.
				 A small meadow bringing you in all rooms. ))
			(citchen (You are in the kitchen.
				 the smell of backed vegetables makes you feel hungry.))
			(sleeping-room (you are in the sleeping room.
				       it looks cozy and the air is cool.))
			(bath-room (you are in the bathroom.
			      the plase looks really tropical and the ceiling glares with beauty.))
			(office (you are in the office room.
					sawdust lays in the corners.))))

(defparameter *edges* '((living-room (hallway west door))
			(hallway (living-room east door)
			 (citchen south opening)no
			 (sleeping-room south-east door)
			 (bath-room west door)
			 (office north door))
			(citchen (hallway north opening))
			(sleeping-room (hallway north door))
			(bath-room (hallway west door))
			(office (hallway south door))))

(defparameter *objects* '(meditation-pillow guitar wormbox jigsaw flatiron watering-can carpet backpack))

(defparameter *object-locations* '((meditation-pillow living-room)
				   (guitar living-room)
				   (wormbox hallway)
				   (jigsaw office)
				   (flatiron sleeping-room)
				   (watering-can sleeping-room)
				   (carpet sleeping-room)
				   (backpack citchen)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun objects-at (loc objs obj-loc)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-loc)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj laying around.)))
  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun walk (dir)
  (let ((next (find dir
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print(lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						   (prin1-to-string lst))
				      'list)
			     t
			     nil)
		 'string))
  (fresh-line))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      (append '(i do not know that command. available commands are - ) *allowed-commands*)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
