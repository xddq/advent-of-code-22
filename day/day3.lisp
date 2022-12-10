;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload "trivia") (ql:quickload :cl-ppcre) (ql:quickload :alexandria))

(defun read-input ()
  (uiop:read-file-lines "./input/day3"))

(defun main ()
  (load-deps)
  (flet ((calc-prio (x) (get-item-type-priority (find-common-type x))))
    (let ((rucksacks (read-input)))
      (values (reduce #'+ (create-compartments rucksacks) :key #'calc-prio)   
              (reduce #'+ (create-three-rucksack-pairings rucksacks) :key #'calc-prio)))))

(main)

(defun create-three-rucksack-pairings (rucksacks)
  "Takes a list of rucksacks and returns a list of lists where the inner lists
  consist of three rucksacks"
  (trivia:match rucksacks
    ((cons one (cons two (cons three xs)))
       (let ((group (list one two three)))
         (if (equal xs nil)
             (list group)
             (concatenate 'list (list group) (create-three-rucksack-pairings xs)))))))

(defun create-compartments (rucksacks)
  "Takes a list of rucksacks and returns a list of lists where the inner lists
  consist of two compartments"
  (mapcar #'create-compartment rucksacks))

(defun create-compartment (rucksack)
  "Takes a rucksack and returns a list containing its 2 compartments."
  (list (subseq rucksack 0 (/ (length rucksack) 2))
                             (subseq rucksack (/ (length rucksack) 2))))

(defun find-common-type (inputlist)
  "Returns the first match where an entry of the needles (first entry in list)
  is contained within each string of the haystacks (restlist)"
  (let ((needles (string-as-char-list (first inputlist)))
        (haystacks (rest inputlist)))
    (let ((result-list
           (mapcar (lambda (needle) (mapcar (lambda (haystack) (find needle haystack))
                                        haystacks))
           needles)))
      (first (reduce (lambda (prev curr) (if (match? prev) prev (if (match? curr) curr)))
             result-list
             :initial-value '(nil))))))

(defun string-as-char-list (given-string)
  (mapcar (lambda (x) (char x 0)) (cl-ppcre:split "(\\s*)" given-string)))

(defun match? (list-with-entries)
  (if (equal (length list-with-entries) 1)
      (not (null (first list-with-entries)))
      (and (not (every #'null list-with-entries))
           (equal (first list-with-entries) (second list-with-entries)))))

; offset when "a" is expected to be value 1
(defun get-char-code-lower-offset ()
  (- 0 (- (char-code (char "a" 0)) 1) ))

; offset when "A" is expected to be value 27
(defun get-char-code-upper-offset ()
  (- 0 (- (char-code (char "A" 0)) 27)))

(defun get-item-type-priority (item-type)
  (let ((char-code-offset (if (lower-case-p item-type)
                              (get-char-code-lower-offset)
                              (get-char-code-upper-offset))))
    (+ (char-code item-type) char-code-offset)))
