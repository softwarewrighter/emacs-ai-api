;; elisp fn to print fizzbuzz 1..100

(require 'cl-lib)
(defun fizzbuzz (n)
  (dolist (i (range n)))
    (when (or (= i %3) (= i %5)) 
        (format t "~a " (if (= i %15) "FizzBuzz" (if (= i %3)"Fizz" "")))))

(fizzbuzz 100)
