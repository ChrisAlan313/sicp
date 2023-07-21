; Use the smallest-divisor procedure to find the smallest divisor of each of
; the following numbers: 199, 1999, 19999.

; Searching for divisors
; Since ancient times, mathematicians have been fascinated by problems
; concerning prime numbers, and many people have worked on the problem of
; determining ways to test if numbers are prime. One way to test if a number is
; prime is to find the number’s divisors. The following program finds the
; smallest integral divisor (greater than 1) of a given number n. It does this
; in a straightforward way, by testing n for divisibility by successive
; integers starting with 2.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; We can test whether a number is prime as follows: n is prime if and only if n
; is its own smallest divisor.

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199) ; prime
(smallest-divisor 1999) ; prime
(smallest-divisor 19999) ; smallest divisor = 7

(prime? 199)
(prime? 1999)
(prime? 19999)
