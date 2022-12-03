;; CAREFUL, your eyes might hurt after this. It is the first time I am touching
;; common lisp!

(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload :alexandria))

(defun read-input ()
  "Reads input from file and creates a list of lists with each inner list
  containing all calories per elf."
  (let ((lines (uiop:read-file-lines "./input/day1")))
     (split-sequence:split-sequence "" lines :test #'equal
                                             :remove-empty-subseqs t)))

(defun main ()
  (load-deps)
  (let ((elves-calories (read-input)))
    ; calculates sum for all calories per elf
    (let ((calories-sum (mapcar (lambda (calories-per-elf)
                          (reduce #'+ calories-per-elf :key #'parse-integer))
                        elves-calories)))
      (let ((calories-sorted (sort calories-sum #'>)))
        ; Returns highest calories (part1) and the sum of the highest three (part2)
        (values (first calories-sorted) (reduce #'+ calories-sorted :end 3))))))

(main)
