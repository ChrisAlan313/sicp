(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; Quoting book:
; Newton’s method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value:
;
; x/y2+2y
; -------
;   3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure.
; End of quote.
;
; Comparing this to Newton's method for square roots, which is based on the
; the fact that if y is an approximation of the suqare root of x, then a better
; approximation is given by the value:
;
; y * x/y
; -------
;   2

(define (cubert x)
  (cubert-iter 1.0 x))

(define (cubert-iter guess x)
  (if (cubert-good-enough? guess x)
      guess
      (cubert-iter (cubert-improve guess x) x)))

; ; x/y^2+2y
; ; -------
; ;   3
(define (cubert-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cubert-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.1))

(define (cube x)
  (* x x x))

(cubert 8)
(cubert 27)