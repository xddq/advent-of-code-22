;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries to be available in code"
  (require "asdf")
  (load "~/quicklisp/setup.lisp")
  (ql:quickload :alexandria))

(defun read-input ()
  (uiop:read-file-lines "./input/day1"))

(defun parse-input (lines)
  "Based on the given lines, returns a list of lists where each inner list
  contains all calories for a given elf"
  ; Creates a list of lists with each inner list containing all calories per elf
  (let ((elves-calories (split-sequence:split-sequence "" lines
                                                       :test #'equal
                                                       :remove-empty-subseqs t)))
    ; converts string values to int for each inner list
    (mapcar (lambda (calories) (mapcar #'parse-integer calories)) elves-calories)))

(defun main ()
  (load-deps)
  (let ((elves-calories (parse-input (read-input))))
    (let ((calories-sum (mapcar
                          (lambda (calories-per-elf) (reduce #'+ calories-per-elf))
                          elves-calories)))
      (let ((calories-sorted (sort calories-sum (lambda (a b) (> a b)))))
        ; Returns the highest calories (part1) and the sum of the highest three
        ; (part2)
        (values (first calories-sorted) (reduce #'+ (subseq calories-sorted 0 3)))
      )
    )
  )
)

(main)
