;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload :alexandria)
  (ql:quickload :cl-ppcre))

(defun read-input ()
  "Reads input from file and creates a list of lists with each inner list
  containing all calories per elf."
  (let ((lines (uiop:read-file-lines "./input/day2")))
    lines))

(defun parse-round (given-round)
  (let ((moves (cl-ppcre:split "(\\s+)" given-round)))
    (list (decrypt-move (first moves)) (decrypt-move (second moves)))))

(defun decrypt-move (encrypted-move)
  "Decrypts the given move. Returns one of '('rock' 'paper' 'scissor')'"
  ; can use member since everything not nil will be evaluated to true
  (cond ((member encrypted-move '("A" "X") :test #'equal) "rock")
        ((member encrypted-move '("B" "Y") :test #'equal) "paper")
        ; TODO figure out how to throw exception, or in general handle error
        ; case (if no condition would match)
        (t "scissor")))

(defun get-shape-value (shape)
  (cond ((equal shape "rock") 1)
        ((equal shape "paper") 2)
        ((equal shape "scissor") 3)))

(defun calculate-round (opponents-move own-move)
  "Calculates the points the own player will get after a Rock Paper Scissors
  round based on the given moves."
  (let ((own-shape-value (get-shape-value own-move))
        (*outcome-win* 6) (*outcome-draw* 3) (*outcome-lose* 0))
    (let ((outcome-value
          (cond ((equal opponents-move own-move) *outcome-draw*)
                ((and (equal opponents-move "rock") (equal own-move "paper")) *outcome-win*)
                ((and (equal opponents-move "paper") (equal own-move "scissor")) *outcome-win*)
                ((and (equal opponents-move "scissor") (equal own-move "rock")) *outcome-win*)
                ((and (equal opponents-move "rock") (equal own-move "scissor")) *outcome-lose* )
                ((and (equal opponents-move "paper") (equal own-move "rock")) *outcome-lose*)
                ((and (equal opponents-move "scissor") (equal own-move "paper")) *outcome-lose*))))
    (+ outcome-value own-shape-value))))

; (defun test-round (opponents-move own-move expected-outcome)
;   (equal (calculate-round opponents-move own-move) expected-outcome))
; (test-round "rock" "paper" 8)
; (test-round "paper" "rock" 1)
; (test-round "scissor" "scissor" 6)

(defun main ()
  (load-deps)
  (let ((rounds (mapcar
                (lambda (current-round) (parse-round current-round))
                (read-input))))
    (let ((calculated-rounds (mapcar
                             (lambda (current-round) (calculate-round (first current-round) (second current-round)))
                             rounds)))
      (reduce #'+ calculated-rounds))))

(main)
