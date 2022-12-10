;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload :alexandria)
  (ql:quickload :cl-ppcre))

(defun read-input ()
  (uiop:read-file-lines "./input/day2"))

(defun main ()
  (load-deps)
  (let ((rounds (mapcar (alexandria:curry #'cl-ppcre:split "(\\s+)") (read-input)))
        (calc-part-1 (alexandria:curry #'calculate-round #'decrypt-part1))
        (calc-part-2 (alexandria:curry #'calculate-round #'decrypt-part2)))
    (values (reduce #'+ rounds :key calc-part-1)
            (reduce #'+ rounds :key calc-part-2))))  

(main)

(defun calculate-round (decrypt-fn current-round)
  (calculate-points (funcall decrypt-fn current-round)))

(defun decrypt-part1(current-round)
  (list (decrypt-move (first current-round)) (decrypt-move (second current-round))))

(defun decrypt-part2 (current-round)
  (let ((opponents-move (decrypt-move (first current-round)))
        (outcome (decrypt-outcome (second current-round))))
    (let ((own-move (get-move :move opponents-move :outcome outcome)))
      (list opponents-move own-move))))

(defun decrypt-move (encrypted-move)
  "Decrypts the given move. Returns one of '('rock' 'paper' 'scissor')"
  ; can use member since everything not nil will be evaluated to true
  (cond ((member encrypted-move '("A" "X") :test #'equal) "rock")
        ((member encrypted-move '("B" "Y") :test #'equal) "paper")
        ; TODO figure out how to throw exception, or in general handle error
        ; case (if no condition would match)
        (t "scissor")))

(defun decrypt-outcome (encrypted-outcome)
  "Decrypts the given outcome. Returns one of '(win lose draw)"
  (cond ((equal encrypted-outcome "X") "lose")
        ((equal encrypted-outcome "Y") "draw")
        ((equal encrypted-outcome "Z") "win")))

(defun calculate-points (moves)
  "Calculates the points the own player will get after a Rock Paper Scissors
  round based on the given moves."
  (let ((opponents-move (first moves)) (own-move (second moves))
        (*outcome-win* 6) (*outcome-draw* 3) (*outcome-lose* 0))
    (let ((own-shape-value (get-shape-value own-move))
          (outcome-value
          (cond ((equal opponents-move own-move) *outcome-draw*)
                ((and (equal opponents-move "rock") (equal own-move "paper")) *outcome-win*)
                ((and (equal opponents-move "paper") (equal own-move "scissor")) *outcome-win*)
                ((and (equal opponents-move "scissor") (equal own-move "rock")) *outcome-win*)
                ((and (equal opponents-move "rock") (equal own-move "scissor")) *outcome-lose* )
                ((and (equal opponents-move "paper") (equal own-move "rock")) *outcome-lose*)
                ((and (equal opponents-move "scissor") (equal own-move "paper")) *outcome-lose*))))
    (+ outcome-value own-shape-value))))

(defun get-shape-value (shape)
  (cond ((equal shape "rock") 1)
        ((equal shape "paper") 2)
        ((equal shape "scissor") 3)))

(defun get-move (&key move outcome)
  (cond ((equal outcome "win") (get-winning-move move))
        ((equal outcome "lose") (get-losing-move move))
        ((equal outcome "draw") (get-draw-move move))))

(defun get-winning-move (opponents-move)
  (cond ((equal opponents-move "rock") "paper")
        ((equal opponents-move "paper") "scissor")
        ((equal opponents-move "scissor") "rock")))

(defun get-losing-move (opponents-move)
  (cond ((equal opponents-move "rock") "scissor")
        ((equal opponents-move "paper") "rock")
        ((equal opponents-move "scissor") "paper")))

(defun get-draw-move (opponents-move) opponents-move)
