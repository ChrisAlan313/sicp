; Exercise 1.3: Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers.

(load "utils.scm")

(define (sum-larger-squares a b c)
  (cond ((and (> a  c) (> b c)) (double-squares a b))
        ((and (> b a) (> c a)) (double-squares b c))
        (else (double-squares a c))))

(define (square x)
  (* x x))

(define (double-squares a b)
  (+ (square a) (square b)))

(test (sum-larger-squares 1 2 3) 13)
(test (sum-larger-squares 1 3 2) 13)
(test (sum-larger-squares 2 1 3) 13)
(test (sum-larger-squares 2 3 1) 13)
(test (sum-larger-squares 3 1 2) 13)
(test (sum-larger-squares 3 2 1) 13)
