; 1. exercises 1.31, 1.32 and 1.33 are in the exercise folder

; 2
(define (every f list)
    (if (empty? list)
        '()
        (se (f (first list)) (every f (bf list)))))

; 3 (ex 7 factorial)
(lambda (fact) (fact 7 fact))
 (lambda (x fact)
    (if (= x 1)
        1
        (* x (fact (- x 1) fact))))