; A function f is defined by the rule that
; f(n)=n if n<3
; and f(n)=f(n−1)+2f(n−2)+3f(n−3) if n≥3.
; Write a procedure that computes f by means of a recursive process. Write a
; procedure that computes f by means of an iterative process.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 12)

; Abandoned the iterative problem and checked the solution on:
; https://sicp-solutions.net/post/sicp-solution-exercise-1-11/
; I'll come back to this at some point later when I've forgotten the solution.
