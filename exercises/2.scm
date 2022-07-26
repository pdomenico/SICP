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
    (if (null? (cdr items))
        (f (car items))
        (and (f (car items)) (new-for-each f (cdr items)))))

; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(define (total-weight mobile)
    (let ((ls (branch-structure (left-branch mobile)))
          (rs (branch-structure (right-branch mobile))))
        (cond ((and (number? ls) (number? rs)) (+ ls rs))
              ((number? rs) (+ rs (total-weight ls)))
              ((number? ls) (+ ls (total-weight rs)))
              (else (+ (total-weight rs) (total-weight ls))))))

(define (total-length branch)
    (if (number? (branch-structure branch))
        (branch-length branch)
        (+ (branch-length branch)
           (total-length (left-branch (branch-structure branch)))
           (total-length (right-branch (branch-structure branch))))))

(define (balanced? mobile)
    (let ((right-weight (if (number? (branch-structure (right-branch mobile)))
                            (branch-structure (right-branch mobile))
                            (total-weight (branch-structure (right-branch mobile)))))
          (left-weight (if (number? (branch-structure (left-branch mobile)))
                            (branch-structure (left-branch mobile))
                            (total-weight (branch-structure (left-branch mobile))))))
        (= (* right-weight (total-length (right-branch mobile)))
           (* left-weight (total-length (left-branch mobile))))))

; Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; New selectors:
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)



; Exercise 2.30

(define make-tree cons)
(define datum car)
(define children cdr)

(define (square-tree tree)
    (define (helper tree squared-children)
        (if (empty? (children tree))
            (make-tree (square (datum tree)) squared-children)
            (helper (make-tree (datum tree) (cdr (children tree)))
                    (append squared-children (list (square-tree (car (children tree))))))))
    (helper tree '()))

; Square tree with higher-order procedures
(define (square-tree-ho tree)
    (make-tree (square (datum tree)) (map square-tree (children tree))))

; Exercise 2.31
(define (tree-map f tree)
    (make-tree (f (datum tree)) (map f (children tree))))

; Exercise 2.36
(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items) (accumulate op init (cdr items)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
(define (matrix-*-vector m v)
    (map (lambda (row)
            (accumulate + 0 (accumulate-n * 1 (list row v))))
         m))

(define (transpose mat)
    (accumulate-n (lambda (x y) 
                    (if (null? y)
                        (list x)
                        (append (list x) y)))
                  '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
            (map (lambda (n-column)
                    (accumulate + 0 (accumulate-n * 1 (list m-row n-column))))
                 cols))
         m)))

; Some additional helper functions
(define (random-list length)
    (define (helper items count)
        (if (= count 0)
            items
            (helper (append (list (random 100)) items) (- count 1))))
    (helper '() length))

(define (random-matrix length height)
    (define (helper rows count)
        (if (= count 0)
            rows
            (helper (append (list (random-list length)) rows) (- count 1))))
    (helper '() height))

(define (print-matrix mat)
    (define (print-row r)
        (display r) (newline))

    (define (print-row-with-recursion mat)
        (display (car mat)) (newline) (print-matrix (cdr mat)))

    (if (null? (cdr mat))
        (print-row (car mat))
        (print-row-with-recursion mat)))