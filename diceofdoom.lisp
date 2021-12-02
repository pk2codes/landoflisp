(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice)) ; setup reinforcement
                             (mod (1+ player) *num-players*) ; change player
                             0
                             t))
            moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                               (when (and (not (eq (player dst) cur-player))
                                          (> (dice src) (dice dst)))
                                 (list
                                  (list (list src dst)
                                        (game-tree (board-attack board cur-player src dst (dice src))
                                                   cur-player
                                                   (+ spare-dice (dice dst))
                                                   nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))





(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                              (second hex))))))
