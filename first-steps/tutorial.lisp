; This was created to understand the basics of common lisp to be able to even
; solve day 1 :D

; following: https://lispcookbook.github.io/cl-cookbook

; function creation
; (defun <name> (list of arguments)
  ; "docstring"
  ; (function body))

(defun hello-world ()
  ;;               ^^ no arguments
  (print "hello world!"))

(defun hello (name)
  "Say hello to `name'."
  (format t "hello ~a !~&" name))

(hello "test")

; optional arguments
; age and gender would be optional arguments here (order has to be correct!)
; (defun hello (name &optional age gender) …)

; named parameters
; avoids having to remember the order, can just specify them by their name.
; values are 'nil' by default.
(defun hello-named-params (name &key (happy t))
  "If `happy' is `t', print a smiley"
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&")))

(hello-named-params "me" :happy t)
(hello-named-params "me" :happy nil)
(hello-named-params "me")

(defun return-the-input (a b c)
  (values a b c))

(return-the-input 1 2 3)

(return-the-input (return-the-input 1 2 3))

(multiple-value-bind (res1 res2 res3)
    (foo :a :b :c)
s (format t "res1 is ~a, res2 is ~a, res2 is ~a~&" res1 res2 res3))

; loads asdf (and UIOP) into lisp
; https://quickdocs.org/uiop
(require "asdf")
(load "~/quicklisp/setup.lisp")
(ql:quickload "iterate")
(ql:quickload "for")
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(uiop:read-file-lines "./test")

(defparameter *mylist* '(1 2))
(defun append-to-list (&key item)
                "Appends the given `item' to the list `mylist'"
                (append (item) *mylist*)
                )

(defparameter mylist '(1 2 3))
(push 0 mylist)

(defun sum (list)
  (reduce (lambda (prev next) (+ prev next)) list))
(sum '(1 2 3))

(defparameter input (uiop:read-file-lines "./test"))
(defparameter elves-calories (split-sequence:split-sequence "" input
                                                            :test #'equal
                                                            :remove-empty-subseqs t))
(print elves-calories)

(defparameter elves-calories-int (mapcar (lambda (elves) (mapcar (lambda (elf-calories) (parse-integer elf-calories)) elves)) elves-calories))
(print test1)

; works!
; (mapcar (lambda (a) (last a)) elves-calories)
(defparameter calories-sum  (mapcar (lambda (a) (reduce (lambda (prev next) (+ prev next)) a)) elves-calories-int))
(reduce #'max calories-sum)

; (defun get-max-with-index (list)
;   (let ((max 0) (index 0)))
;   (for:for ((x over) list)
;     (if (> x max)
;         (setf max x)
;         (setf index)
;     )
;     (incf index)
;   )
;   )

(< nil 3)


; works when using the #' syntax to get the 'function object' based on a name
; https://gigamonkeys.com/book/functions.html
(defun test (a)
  (* 10 a))
(mapcar (lambda (a) (* 10 a)) '(1 2 3 4))
(mapcar #'test '(1 2 3 4))

; (split-sequence:split-sequence "" '("1" "" "2")
;                                :test #'equal
;                                :remove-empty-subseqs t)

; sample for mapping over a list
; src: https://lispcookbook.github.io/cl-cookbook/data-structures.html
(defparameter sample-list '(1 2 3))
(map 'list (lambda (it) (* 10 it)) sample-list)

; (reduce (lambda (prev next)) (+ prev next) '(1 2 3 4))
(reduce (lambda (prev next) (+ prev next)) '(1 2 3 4))
(reduce (lambda (prev next) (+ prev next)) '(1 2 3 4))
('('(1 2) '(3 4)))

; for iteration
; src: https://lispcookbook.github.io/cl-cookbook/iteration.html
(for:for ((x over '(1 2)))
   (print x))

; learn if else and collect
;; https://riptutorial.com/common-lisp/example/11095/conditionally-executing-loop-clauses
; https://lispcookbook.github.io/cl-cookbook/iteration.html
(loop repeat 10
      for x = (random 100)
      if (evenp x)
        collect x into evens
      else
        collect x into odds
      finally (return (values evens odds)))

; creates closure to capture lines we have read
(let ((elves '()) (elves-count 1))
  (for:for ((x over (uiop:read-file-lines "./test")))
    ; if (not (alexandria:emptyp x))
    (if (not (alexandria:emptyp x))
      (push x elves)
      ; this seems to be the else case
      (incf elves-count)
        )
    ; else
    ;   (incf elves-count))
  )
  (print elves)
  (print elves-count)
  )

; can be used to create list of list
(list '(1 2) '(3 4) '(5 6))

; closure test
; src: https://lispcookbook.github.io/cl-cookbook/functions.html#closures
(defun repeater (n)
  (let ((counter -1))
     (lambda ()
       (if (< counter n)
         (incf counter)
         (setf counter 0)))))
(defparameter *my-repeater* (repeater 3))
(funcall *my-repeater*)

