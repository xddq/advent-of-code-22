(defun load-deps ()
  "Loads libraries required for the rest of the code"
  (ql:quickload :alexandria)
  (ql:quickload :cl-ppcre))

(defun read-input ()
  (uiop:read-file-lines "./input/day4"))

;day1
(defun main ()
  (load-deps)
  (reduce #'+ (read-input) :key #'contain-each-other))

(main)

(defun contain-each-other (line)
  (multiple-value-bind (assignment-1 assignment-2) (line-to-assignments line)
    (let ((min-1 (first assignment-1))
          (max-1 (second assignment-1))
          (min-2 (first assignment-2))
          (max-2 (second assignment-2)))
      (if (or  (and (<= min-1 min-2) (>= max-1 max-2))
               (and (<= min-2 min-1) (>= max-2 max-1))) 1 0))))

(defun line-to-assignments (line)
  (let ((assignment-as-list (mapcar #'get-int-from-string
                                      (alexandria:flatten (mapcar
                                        (lambda (a) (cl-ppcre:split "(-)" a))
                                        (cl-ppcre:split "(,)" line))))))
    (values (subseq assignment-as-list 0 2) (subseq assignment-as-list 2 4))))

(defun get-int-from-string (given-string)
  (multiple-value-bind (parsed-integer _) (parse-integer given-string)
    parsed-integer))
