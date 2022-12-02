;; CAREFUL, your eyes might hurt after this. It iss the first time I am touching
;; common lisp!

(defun main ()
  ; loads libraries into common lisp using quickload
  (require "asdf")
  (load "~/quicklisp/setup.lisp")
  (ql:quickload "iterate")
  (ql:quickload "for")
  (ql:quickload :alexandria)
  (defparameter input (uiop:read-file-lines "./input/day1"))
  (defparameter elves-calories (split-sequence:split-sequence "" input
                                                            :test #'equal
                                                            :remove-empty-subseqs t))
  ; converts list of list of strings to list of list of integers
  (defparameter elves-calories-int (mapcar (lambda (elves) (mapcar (lambda (elf-calories) (parse-integer elf-calories)) elves)) elves-calories))
  ; calculates sum for each list in the list of lists
  (defparameter calories-sum  (mapcar (lambda (a) (reduce (lambda (prev curr) (+ prev curr)) a)) elves-calories-int))
  ; sorts largest first
  (defparameter sorted-calories (sort calories-sum (lambda (a b) (> a b))))
  (first sorted-calories)
  (defparameter largest-three (subseq sorted-calories 0 3))
  (defparameter total-of-largest-three ( reduce (lambda (prev curr) (+ prev curr)) largest-three))
  ; returns multiple values from function to get both solutions
  (values (first sorted-calories) total-of-largest-three)
  )


(main)
