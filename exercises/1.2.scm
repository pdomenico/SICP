; Exercise 1.11 
; A function f is defined by the rule that: 
; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.

; recursive
(define (recursive-f n)
    (cond ((< n 3) n)
          (else (+ (recursive-f (- n 1))
                   (* 2 (recursive-f (- n 2)))
                   (* 3 (recursive-f (- n 3)))))))

; iterative
(define (iterative-f n)
    (define (iter sum last second-last count)
        (cond ((> count n) sum)
              ((= count 1) (iter 1 0 0 (+ 1 count)))
              ((= count 2) (iter 2 1 0 (+ 1 count)))
              (else (iter (+ sum (* 2 last) (* 3 second-last))
                          sum
                          last
                          (+ 1 count)))))
    (iter 0 0 0 1))