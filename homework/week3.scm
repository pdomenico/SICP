; Exercises are in exercises/1.2 and 1.3 files

; Homework N2
(define (sum-of-factors n)
    (define (helper m sum)
        (cond ((= m n) sum)
              ((= 0 (modulo n m)) (helper (+ 1 m) (+ m sum)))
              (else (helper (+ 1 m) sum))))
    (helper 1 0))

(define (next-perf n)
    (define (test m)
        (if (= m (sum-of-factors m))
            m
            (test (+ 1 m))))
    (test n))