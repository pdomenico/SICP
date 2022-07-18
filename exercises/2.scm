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

; Exercise 2.5
(define (exp base exponent)
    (define (iter base exponent a)
        (cond ((= exponent 0) 1)
              ((= exponent 1) (* base a))
              ((= 0 (modulo exponent 2)) (iter (square base) (/ exponent 2) a))
              (else (iter base (- exponent 1) (* a base)))))
    (iter base exponent 1))

(define (pair a b)
    (* (exp 2 a) (exp 3 b)))

(define (first-of-pair p)
    (define (iter a count)
        (if (= 0 (modulo a 2))
            (iter (/ a 2) (+ 1 count))
            count))
    (iter p 0))

(define (second-of-pair p)
    (define (iter a count)
        (if (= 0 (modulo a 3))
            (iter (/ a 3) (+ 1 count))
            count))
    (iter p 0))

; Exercise 2.7-2.8
(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

; Exercise 2.10
(define (div-interval x y)
    (if (and (<= (lower-bound y) 0)
             (>= (upper-bound y) 0))
        (error "Divisor can't span 0!")
        (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
    (make-interval (- center (/ (* center percent) 100))
                   (+ center (/ (* center percent) 100))))

(define (percent i)
    (/ (* (- (center i) (lower-bound i))
          100)
       (center i)))

; Exercise 2.17
(define (last-pair list)
    (if (null? (cdr list))
        (car list)
        (last-pair (cdr list))))

; Exercise 2.20
(define (same-parity . l)
    (define (helper arg)
        (cond ((null? (cdr arg))
               (if (= (modulo (car arg) 2) (modulo (car l) 2))
                   (list (car arg))
                   (cdr arg)))
              ((= (modulo (car arg) 2) (modulo (car l) 2))
               (append (list (car arg)) (helper (cdr arg))))
              (else (helper (cdr arg)))))
    (helper l))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

; Exercise 2.21
(define (new-for-each f items)
    (f (car items))
    (if (null? (cdr items))
        #t
        (new-for-each f (cdr items))))
