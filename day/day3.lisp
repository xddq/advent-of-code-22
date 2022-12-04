;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload "trivia")
  (ql:quickload :cl-ppcre))

(defun read-input ()
  (uiop:read-file-lines "./input/day3"))

(defun get-priority-for-rucksack (rucksack)
  (get-item-type-priority (find-common-type (create-compartments rucksack))))

(defun get-priority-for-three-rucksacks (three-rucksacks)
  (get-item-type-priority (find-common-type-three-rucksacks three-rucksacks)))

(defun string-as-char-list (given-string)
  (cl-ppcre:split "(\\s*)" given-string))

(defun create-compartments (rucksack)
  "Takes a rucksack and returns a list containing its 2 compartments."
  (list (subseq rucksack 0 (/ (length rucksack) 2))
                             (subseq rucksack (/ (length rucksack) 2))))

(defun find-common-type-two-compartments (compartments-of-a-rucksack)
  (let ((first-compartment (first compartments-of-a-rucksack))
        (second-compartment (second compartments-of-a-rucksack)))
    (reduce #'concat-strings (mapcar
                              (lambda (item) (if (find (char item 0) second-compartment :test #'equal) item ""))
                              (string-as-char-list first-compartment)))))

(defun concat-strings (a b)
  (concatenate 'string a b))

; offset when "a" is expected to be value 1
(defun get-char-code-lower-offset ()
  (- 0 (- (char-code (char "a" 0)) 1) ))

; offset when "A" is expected to be value 27
(defun get-char-code-upper-offset ()
  (- 0 (- (char-code (char "A" 0)) 27)))

(defun get-item-type-priority (item-type)
  (let ((item-as-char (char item-type 0)))
    (let ((char-as-code (char-code item-as-char)))
      (if (lower-case-p item-as-char)
          (+ char-as-code (get-char-code-lower-offset))
        (+ char-as-code (get-char-code-upper-offset))))))

(defun solve-day1 ()
  (reduce #'+ (read-input) :key #'get-priority-for-rucksack))

(defun solve-day2 ()
  (reduce #'+ (create-three-rucksack-pairings (read-input))
    :key #'get-priority-for-three-rucksacks))

(defun create-three-rucksack-pairings (rucksacks)
  "Takes a list of rucksacks and returns a list of lists where the inner lists
  consist of three rucksacks"
  (trivia:match rucksacks
    ((cons one (cons two (cons three xs)))
       (let ((group (list one two three)))
         (if (equal xs nil)
               (list group)
             (concatenate 'list (list group) (create-three-rucksack-pairings xs)))))))

(defun find-common-type-three-rucksacks (three-rucksacks)
  (let ((rucksacks (mapcar #'string-as-char-list three-rucksacks)))
    (let ((first-rucksack (first rucksacks))
          (second-rucksack (second rucksacks))
          (third-rucksack (third rucksacks)))
      (reduce #'concat-strings (mapcar (lambda (item) (if (and (find item second-rucksack :test #'equal) (find item third-rucksack :test #'equal)) item "")) first-rucksack)))))

(defun main ()
  (load-deps)
  (values (solve-day1) (solve-day2)))

(main)
