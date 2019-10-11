(defparameter *width* 100)
(defparameter *height* 60)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 500)
(defparameter *reproduction-energy* 200)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *energy-coeff* 100) ;; describes tanh function for food consumption efficiency
(defparameter *movement-penalty-coeff* 500)
(defparameter *max-movement-penalty* 15)

(defun movement-genes (animal)
  (subseq (animal-genes animal) 0 8))

(defun energy-efficiency-meat-gene (animal)
  (nth 8 (animal-genes animal)))

(defun energy-efficiency-veg-gene (animal)
  (nth 9 (animal-genes animal)))

(defun affinity-meat-gene (animal)
  (nth 10 (animal-genes animal)))

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
  (let ((veg-effiency (energy-effiency (energy-efficiency-veg-gene animal)))
	(meat-effiency (energy-effiency (energy-efficiency-meat-gene animal))))
    (round (* (tanh (/ (+ veg-effiency meat-effiency) *movement-penalty-coeff*)) *max-movement-penalty*))))

(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1)
					  ((or (= dir 1) (= dir 5)) 0)
					  (t -1)))
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0)))
				 *height*))
    (decf (animal-energy animal))))

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

(defun energy-effiency (energy-efficiency-gene)
  (round (* (tanh (/ energy-efficiency-gene *energy-coeff*)) *plant-energy*)))

(defun eat-plant (animal)
  (incf (animal-energy animal)
	(energy-effiency (energy-efficiency-veg-gene animal))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (eat-plant animal)
      (remhash pos *plants*))))

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

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal) (<= (animal-energy animal) 0))
			     *animals*))
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
		  do (princ (cond ((some (lambda (animal)
					   (and (= (animal-x animal) x)
						(= (animal-y animal) y)))
					 *animals*)
				   (animal-symbol animal))
				  ((gethash (cons x y) *plants*) #\#)
				  (t #\space))))
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
