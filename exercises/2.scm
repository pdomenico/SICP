; Exercise 2.1
(define numer car)
(define denom cdr)
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (let ((g (abs (gcd n d))))
        (if (< d 0)
            (cons (- 0 (/ n g)) (- 0 (/ d g)))
            (cons (/ n g) (/ d g)))))

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))

; Exercise 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (average a b) (/ (+ a b) 2))

(define (midpoint segment)
    (cons (average (x-point (start-segment segment)) (x-point (end-segment segment)))
          (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

; Exercise 2.3
(define make-rectangle cons)
(define length car)
(define height cdr)

(define (distance-points a b)
    (sqrt (+ (square (- (x-point a) (x-point b)))
             (square (- (y-point a) (y-point b))))))

(define (segment-length s) (distance-points (start-segment s) (end-segment s)))

(define (make-rectangle-points a b c d)
    (let ((ab (distance-points a b))
          (bc (distance-points b c))
          (cd (distance-points c d))
          (ad (distance-points a d)))
        (if (and (= ab cd) (= ad bc))
            (cons (cons (make-segment a b) (make-segment c d))
                  (cons (make-segment b c) (make-segment a d))))))

(define (perimeter rectangle)
    (+ (* 2 (segment-length (car (car rectangle))))
       (* 2 (segment-length (car (cdr rectangle))))))

(define (area rectangle)
    (* (segment-length (car (car rectangle)))
       (segment-length (car (cdr rectangle)))))

(define (make-rectangle-segments base1 height1 base2 height2)
    (cons (cons base1 height1) (cons base2 height2)))




