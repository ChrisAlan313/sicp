
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) 
          (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define two-thirds (make-rat 2 3))

; Exercise 2.1:
; Define a better version of make-rat that handles both positive and negative
; arguments. Make-rat should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) 
          (/ d g))))

; It already does that. Example:
(print-rat (make-rat 4 -5))
; returns -4/5

; Exercise 2.2:
; Consider the problem of representing line segments in a plane. Each segment
; is represented as a pair of points: a starting point and an ending point.
; Define a constructor make-segment and selectors start-segment and end-segment
; that define the representation of segments in terms of points. Furthermore, a
; point can be represented as a pair of numbers: the x coordinate and the y
; coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your
; selectors and constructors, define a procedure midpoint-segment that takes a
; line segment as argument and returns its midpoint (the point whose
; coordinates are the average of the coordinates of the endpoints). To try your
; procedures, you’ll need a way to print points:

; Exercise broken down:
;   [x] define constructor make-point
;   [x] selectors x-point and y-point
;   [x] define constructor make-segment
;   [x] define selectors start-segment and end-segment
;   [x] define midpoint-segment that takes a line segment as argument and
;       returns the average of the coordinates of the endpoints

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  ; (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; car = start-segment, that is point a
; cdr = end-segment, that is point b
(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-segment segment)
  (newline)
  (print-point (start-segment segment))
  (display "->")
  (print-point (end-segment segment)))

; (print-segment (make-segment (make-point 2 4)
;               (make-point 6 8)))

; returns (2,4)->(6,8)

; Contains the secret sauce to prevent fractions
(define (avg-of-two a b)
  (/ (+ a b) 2.0))

; For (a,b)->(x,y), do (avg a x), (avg b y)
(define (midpoint-segment segment)
  (let ((ss (start-segment segment)))
    (let ((es (end-segment segment)))
      (make-point (avg-of-two (x-point ss) (x-point es))
                  (avg-of-two (y-point ss) (y-point es))))))

; Should be (4,6) from (2,4)->(6,8)
; (print-point
;   (midpoint-segment
;     (make-segment (make-point 2 4)
;                   (make-point 6 8))))
; Returns (4,6)

; Should be (-0.5,2.5) from (-3,0)->(2,5)
; (print-point
;   (midpoint-segment
;     (make-segment (make-point -3 0)
;                   (make-point 2 5))))
; Returns (-.5,2.5)

; Exercise 2.3:
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of Exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given
; rectangle. Now implement a different representation for rectangles. Can you
; design your system with suitable abstraction barriers, so that the same
; perimeter and area procedures will work using either representation?

; So there are two kinds of rectangles that can be made here, one easier and
; one harder. The easier is the rectangle who's sides run parallel with x and y
; axes. Let's do that as the first representation, then the form that runs
; cattycorner to the axes secondly. We can also throw a validator into the
; make-rect if time, energy and motivation allow.

; A rectangle is made of four lines (or "segments"). Going clockwise with
; points a, b, c and d, those are segment a-b, b-c, c-d and d-a. You could also
; make it with four segments, but that means redundantly using each value twice
; rather than just naming each vertices. Perhaps provide an adapter procedure
; that gives a user the option to validate and decompose four segments into
; four valid vertices of a rectangle.

(define (make-rect vertex-a vertex-b vertex-c vertex-d)
  (cons (cons (make-segment vertex-a vertex-b)
              (make-segment vertex-b vertex-c))
        (cons (make-segment vertex-c vertex-d)
              (make-segment vertex-d vertex-a))))

(define (a-b-segment rect)
  (car (car rect)))

(define (b-c-segment rect)
  (cdr (car rect)))

(define (c-d-segment rect)
  (car (cdr rect)))

(define (d-a-segment rect)
  (cdr (cdr rect)))

(define (print-rect rect)
  (print-segment (a-b-segment rect))
  (print-segment (b-c-segment rect))
  (print-segment (c-d-segment rect))
  (print-segment (d-a-segment rect))
  rect)

(define (rect)
  (make-rect (make-point 0 2)
             (make-point 4 2)
             (make-point 4 0)
             (make-point 0 0)))

; distance=√((x1−x2)^2+(y1−y2)^2)
(define (distance point-a point-b)
  (sqrt (+ (square (- (x-point point-a) (x-point point-b)))
           (square (- (y-point point-a) (y-point point-b))))))

(define (seg-length segment)
  (distance (start-segment segment) (end-segment segment)))

(define (rect-perimeter rect)
  (+ (seg-length (a-b-segment rect))
     (seg-length (b-c-segment rect))
     (seg-length (c-d-segment rect))
     (seg-length (d-a-segment rect))))

(rect-perimeter (make-rect (make-point 0 8)
                           (make-point 2 8)
                           (make-point 2 0)
                           (make-point 0 0)))

(define (rect-area rect)
  (* (seg-length (a-b-segment rect))
     (seg-length (b-c-segment rect))))

(rect-area (make-rect (make-point 0 8)
                      (make-point 2 8)
                      (make-point 2 0)
                      (make-point 0 0)))

; Ending the exercise here. As always, this requires more time than expected
; and I need to move on.
