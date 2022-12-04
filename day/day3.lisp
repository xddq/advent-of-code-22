;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload :cl-ppcre))

(defun read-input ()
  (uiop:read-file-lines "./input/day3"))

(defun main ()
  (load-deps)
  (reduce #'+ (read-input) :key #'get-priority-for-rucksack))

(main)

(defun get-compartments (rucksack)
  "Returns the compartments as list of item types for each compartment in the
  given rucksack"
  (let ((compartments (list (subseq rucksack 0 (/ (length rucksack) 2))
                             (subseq rucksack (/ (length rucksack) 2)))))
    (mapcar #'compartment-as-list compartments)))

(defun compartment-as-list (compartment)
  (cl-ppcre:split "(\\s*)" compartment))

(defun find-common-type (compartments-of-a-rucksack)
  (let ((first-compartment (first compartments-of-a-rucksack))
        (second-compartment (second compartments-of-a-rucksack)))
    (reduce #'concat-strings (mapcar (lambda (item) (if (find item second-compartment :test #'equal) item "")) first-compartment))))

(defun concat-strings (a b)
  (concatenate 'string a b))

; offset when "a" is expected to be value 1
(defun get-char-code-lower-offset ()
  (- 0 (- (char-code (char "a" 0)) 1) ))

; offset when "A" is expected to be value 27
(defun get-char-code-upper-offset ()
  (- 0 (- (char-code (char "A" 0)) 27)))

; get prio based on char
(defun get-item-type-priority (item-type)
  (let ((item-as-char (char item-type 0)))
    (let ((char-as-code (char-code item-as-char)))
      (if (lower-case-p item-as-char)
          (+ char-as-code (get-char-code-lower-offset))
        (+ char-as-code (get-char-code-upper-offset))))))

; tests prio function for lower and uppercase
(and (equal (get-item-type-priority "L") 38)
     (equal (get-item-type-priority "p") 16))

(defun find-match (item compartment)
  (if (find item compartment :test #'equal) item ""))

(defun get-priority-for-rucksack (rucksack)
  (get-item-type-priority (find-common-type (get-compartments rucksack))))
