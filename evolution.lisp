(defparameter *width* 100)
(defparameter *height* 60)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 500)
(defparameter *reproduction-energy* 200)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *energy-coeff* 100) ;; describes tanh function for food consumption efficiency
(defparameter *movement-penalty-coeff* 500)
(defparameter *max-movement-penalty* 15)
(defparameter *animal-positions* (make-hash-table :test #'equal))

(defun movement-genes (animal)
  (subseq (animal-genes animal) 0 8))


(defun energy-efficiency-meat-gene (animal)
  (nth 8 (animal-genes animal)))

(defun energy-efficiency-veg-gene (animal)
  (nth 8 (animal-genes animal)))

(defun affinity-meat-gene (animal)
  (nth 9 (animal-genes animal)))

(defun affinity-veg-gene (animal)
  (nth 11 (animal-genes animal)))

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;; x, y: positions
;; energy: representing days before starving
;; dir:
;; 0  1  2
;; 7  A  3
;; 6  5  4
;; genes: '(g0, g1, g2, g3, g4, g5, g6, g7) representing "tendency" to move in a given dir
;; g8 energy efficiency / meat
;; g9 energy efficiency / veggies
;; g10 affinity / meat,
;; g11 affinity / veggies
(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
		     :y (ash *height* -1)
		     :energy 1000
		     :dir 0
		     :genes (concatenate 'list (loop repeat 8 
				collecting (1+ (random 20))) '(6 6 6 6) ))))

(defun move-penalty (animal)
  (let ((veg-effiency (energy-effiency (energy-efficiency-veg-gene animal) *plant-energy* *energy-coeff*))
	;; not quite optimal but thinking is in progress ;)
	(meat-effiency (energy-effiency (energy-efficiency-meat-gene animal) *plant-energy* *energy-coeff*)))
    (round (* (tanh (/ (+ veg-effiency meat-effiency) *movement-penalty-coeff*)) *max-movement-penalty*))))

(defun move (animal)
  (let* ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal))
	(new-x (mod (+ x
		       (cond ((and (>= dir 2) (< dir 5)) 1)
			     ((or (= dir 1) (= dir 5)) 0)
			     (t -1)))
		    *width*))
	(new-y (mod (+ y
		       (cond ((and (>= dir 0) (< dir 3)) -1)
			     ((and (>= dir 4) (< dir 7)) 1)
			     (t 0)))
		    *height*))
	(new-pos (cons new-x new-y))
	(pos (cons x y)))
    
    (setf (animal-x animal) new-x)
    (setf (animal-y animal) new-y)
    (remhash pos *animal-positions*)
    (setf (gethash new-pos *animal-positions*) t)
    (princ pos)
    (decf (animal-energy animal))))
;; todo: remove more energy depending on genes

(defun angle (genes x)
  (let ((xnu (- x (car genes))))
    (if (< xnu 0)
	0
	(1+ (angle (cdr genes) xnu)))))

(defun turn (animal)
  (let ((x (random (apply #'+ (movement-genes animal)))))
    (setf (animal-dir animal)
	    (mod (+ (animal-dir animal) (angle (movement-genes animal) x))
		 8))))

(defun energy-effiency (energy-efficiency-gene energy-cap coeff)
  (round (* (tanh (/ energy-efficiency-gene coeff)) energy-cap)))

(defun increase-energy (animal energy)
  (incf (animal-energy animal) energy))

(defun eat-plant (animal pos)
  (let ((new-energy
	 (energy-effiency (energy-efficiency-veg-gene animal) *plant-energy* *energy-coeff*)))
    (increase-energy animal new-energy)
    (remhash pos *plants*)))

(defun find-animal-by-pos (animal pos)
  (and (= (animal-x animal) (car pos)) (= (animal-y animal) (cdr pos))))

(defun eat-animal (animal-eating pos)
  (let* ((animal-to-eat
	 (find-if (lambda (animal) (find-animal-by-pos animal pos))
		  *animals*))
	(raw-energy (animal-energy animal-eating))
	(netto-energy (energy-effiency
		       (energy-efficiency-meat-gene animal-to-eat) raw-energy *energy-coeff*)))
    (increase-energy animal-eating netto-energy)
    (remove-animal animal-to-eat)))

(defun eat-veg-prob (animal)
  (let* ((meat-affinity (affinity-meat-gene animal))
	 (veg-affinity (affinity-veg-gene animal))
	 (eat-plant-prob (/ veg-affinity (+ veg-affinity meat-affinity))))
    (> (* 100 eat-plant-prob) (random 100))))


(defun eat (animal)
  (let* ((pos (cons (animal-x animal) (animal-y animal)))
	 (has-plant (gethash pos *plants*))
	 (has-animal (gethash pos *animal-positions*))
	 (is-eating-veg (eat-veg-prob animal)))
    (cond ((and has-animal (not is-eating-veg)) (eat-animal animal pos))
	  ((and has-plant is-eating-veg) (eat-plant animal pos)))))

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
	    (genes (copy-list (animal-genes animal)))
	    (mutation (random 12)))
	(setf (nth mutation genes)
	      (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))

(defun remove-animal (animal)
  (let* ((x (animal-x animal))
	 (y (animal-y animal))
	 (pos (cons x y)))
    (remhash pos *animal-positions*)))

(defun starved-animal-p (animal)
  (<= (animal-energy animal) 0))

(defun remove-animals ()
  (let ((starved-animals
	 (remove-if-not #'starved-animal-p *animals*))
	(living-animals
	 (remove-if  #'starved-animal-p *animals*)))
    (loop for starved-animal in starved-animals
       do (remove-animal starved-animal))
    (setf *animals* living-animals)))


(defun update-world ()
  (remove-animals)
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))

(defun animal-symbol (animal)
  (if (> (energy-efficiency-meat-gene animal) (energy-efficiency-veg-gene animal))
      #\m
      #\p))

(defun draw-world ()
  (loop for y
     below *height*
     do (progn (fresh-line)
	       (princ "|")
	       (loop for x
		  below *width*
		  do (let ((animal?
			    (find-if (lambda (animal)
				       (and (= (animal-x animal) x)
					    (= (animal-y animal) y))) *animals*)))
		       (princ (cond ((not (null animal?)) (animal-symbol animal?))
				    ((gethash (cons x y) *plants*) #\#)
				    (t #\space)))
		       ))
	       (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
		      below x
		      do (update-world)
		      if (zerop (mod i 1000))
		      do (princ "."))
		   (update-world))
	       (evolution))))))


;; edit

(defun euclidean-distance-map (p q)
  (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) (mapcar #'- p q)))))
