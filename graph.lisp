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
			 (citchen south opening)
			 (sleeping-room south-east door)
			 (bath-room west door)
			 (office north door))
			(citchen (hallway north opening))
			(sleeping-room (hallway north door))
			(bath-room (hallway west door))
			(office (hallway south door))))

(defun dot-name (exp)
  "converting names into DOT format. Replacing non alphanumerical with _"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


(defparameter *max-label-length* 30)

(defun dot-label (exp)
  "generates the label that should appear in the node when its drawn, cuts text > max-label-length"
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

(defun nodes->dot (nodes)
  "generates name and a label for a node"
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))
(defun edges->dot (edges)
  "generates dot information for edges that link nodes"
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
  "prints nodes an edges in graphviz format on the console "
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  "catches the output of thunk and redirects it to a file and transforms the file via graphviz"
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (uiop:run-program  (list "dot" "-Tpng" "-O" fname)))

(defun graph->png (fname nodes edges)
  "takes a nodes and edges list and saves these in graphviz format as a file"
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

