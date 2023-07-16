; The following pattern of numbers is called Pascal’s triangle.
;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1
;        . . .
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal’s triangle by means of a recursive process.

; Define the base case (row 1 is 1)
; From there we know that either the first column of every row is 1 and the
; last column of every row is 1. The last column of every row is where the
; column number equals the row number. With this, we have the sides of the
; triangle which are all 1's.
;
; Thinking of it this way helps:
; column 1 | column 2 | column 3 | column 4 | column 5
;     1                                               ; row 1
;     1         1                                     ; row 2
;     1         2          1                          ; row 3
;     1         3          3          1               ; row 4
;     1         4          6          4          1    ; row 5
;
(define (pascal row column)
  (cond ((= column 1) 1)
        ((= column row) 1)
        (else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column)))))

(pascal 1 1)
(pascal 2 1)
(pascal 2 2)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)
(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)
