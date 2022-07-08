(load "stop-at-17")
(load "twenty-one")
(load "best-total")

(define (play-n strategy n)
    (define (iter n score)
        (if (= 0 n)
            score
            (iter (- n 1) (+ score (twenty-one strategy)))))
    (iter n 0))