; Exercise 1.29
(define (sum1.2 term a next b)
    (if (> a b)
        0
        (cond ((= a 0)
               (+ (term a)
                  (sum1.2 term (next a) next b)))
              ((= 0 (modulo a 2))
               (+ (* 2 (term a))
                  (sum1.2 term (next a) next b)))
              (else (+ (* 4 (term a))
                       (sum1.2 term (next a) next b))))))


(define (simpson f a b n)
    (define h (/ (- b a) n))
    
    (define (term x)
        (f (+ a (* x h))))
    
    (define (next x)
        (+ x 1))
    
    (* (sum1.2 term 0 next n) (/ h 3)))

(define (cube x)
    (* x x x))

; Exercise 1.30
(define (sum1.3 term a next b)
  (define (iter a result)
    (if (= a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
(define (product a b next f)
    (if (> a b)
        1
        (* (f a) (product (next a) b next f))))

(define (factorial x)
    (define (next x) (+ x 1))
    (define (f x) x)
    (product 1 x next f))

(define (approximatePi x)
    (define (next x) (+ 2 x))
    (define (f x) (* x x))
    (if (= 0 (modulo x 2))
        (* 4.0
           (/ (* 2 (product 4 x next f))
              (* x (product 3 (- x 1) next f))))
        (* 4.0
           (/ (* 2 (product 4 (+ x 1) next f))
              (* x (product 3 x next f))))))

; Exercise 1.32
(define (accumulate combiner null-value a b next f)
    (if (> a b)
        null-value
        (combiner (f a) (accumulate combiner null-value (next a) b next f))))

(define (sumAsAccumulate a b next f)
    (accumulate + 0 a b next f))

(define (productAsAccumulate a b next f)
    (accumulate * 1 a b next f))

; Exercise 1.33
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
    (define (good-enough? guess improved-guess)
        (if (> guess improved-guess)
            (< (/ guess improved-guess) 1.001)
            (< (/ improved-guess guess) 1.001)))

    (define (sqrt-iter guess)
        (if (good-enough? guess (improve guess))
            guess
            (sqrt-iter (improve guess))))

    (define (improve guess)
        (average guess (/ x guess)))

    (sqrt-iter 1.0))

(define (prime? n)
    (define (furtherCheck)
        (define target (sqrt n))
        (define (iter i)
            (cond ((> i target) #t)
                  ((or (= 0 (modulo n i))
                       (= 0 (modulo n (+ i 2)))) #f)
                  (else (iter (+ i 6)))))
    (iter 5))

    (cond ((= n 0) #f)
          ((= n 1) #f)
          ((= n 2) #t)
          ((= n 3) #t)
          ; check if it's divisible by 2 or 3
          ((or (= 0 (modulo n 2))
               (= 0 (modulo n 3))) #f)
          ; check 
          (else (furtherCheck))))

(define (filtered-accumulate combiner null-value a b next f filter)
    (cond ((> a b) null-value)
          ((not (filter a)) (combiner null-value 
                                      (filtered-accumulate combiner null-value (next a) b next f filter)))
          (else (combiner (f a) (filtered-accumulate combiner null-value (next a) b next f filter)))))

; sum of the squares of the prime numbers
(define (sumSquaredPrimes a b)
    (define (next x) (+ x 1))
    (define (f x) (* x x))
    (define (filter x) (prime? x))
    (filtered-accumulate + 0 a b next f filter))

; Exercise 1.40
(define (fixed-point f first-guess)
    (define tolerance 1.0000001)
    (define (goodenough? g1 g2)
        (if (> g1 g2)
            (< (/ g1 g2) tolerance)
            (< (/ g2 g1) tolerance)))
    
    (define (try guess)
        (let ((next (f guess)))
             (if (goodenough? guess next)
                 next
                 (try next))))
    (try first-guess))

(define (sqrtFixed x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (derivative f)
    (define dx 0.00001)
    (lambda (x) (/ (- (f (+ x dx)) (f x))
                   dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derivative g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
    (lambda (x) (+ (* x x x)
                   (* a x x)
                   (* b x)
                   c)))


; Exercise 1.41
(define (double f)
    (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
