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

; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses successive squaring
; and uses a logarithmic number of steps, as does fast-expt.
; (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, along with the exponent n and the base b,
; an additional state variable a, and define the state transformation in such a way that
; the product a bn is unchanged from state to state.
; At the beginning of the process a is taken to be 1, and the answer is given 
; by the value of a at the end of the process. 
; In general, the technique of defining an invariant quantity that remains unchanged from state to state
; is a powerful way to think about the design of iterative algorithms.)

(define (even? n) (= 0 (remainder n 2)))
(define (square n) (* n n))

; Recursive version of fast-exp
(define (recursive-fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Iterative version of fast-exp
(define (iter-fast-expt b n)
    (define (iter a b n)
        (cond ((= n 0) 1)
              ((= n 1) (* a b))
              ((even? n) (iter a (square b) (/ n 2)))
              (else (iter (* a b) b (- n 1)))))
    (iter 1 b n))



; Exercise 1.17
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (times a b)
    (cond ((= b 1) a)
          ((even? b) (double (times a (halve b))))
          (else (+ a (times a (- b 1))))))

; Exercise 1.18
(define (iter-times a b)
    (define (iter a s b)
        (cond ((= b 1) (+ a s))
              ((even? b) (iter (double a) s (halve b)))
              (else (iter a (+ s a) (- b 1)))))
    (iter a 0 b))