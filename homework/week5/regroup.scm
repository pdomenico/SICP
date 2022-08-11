; selects the nth elements of a list
(define (selectn n items)
    (if (= n 1)
        (car items)
        (selectn (- n 1) (cdr items))))

; deletes first n elements from a list
(define (butn n arg)
    (if (= n 0)
        arg
        (butn (- n 1) (cdr arg))))

; selects second last element of a list
(define (secondlast args) (last (bl args)))

; selects third last element of a list
(define (thirdlast args) (last (bl (bl args))))

; selects last 3 elements of a list
(define (last3 items)
    (if (= 3 (count items)) items (last3 (cdr items))))

; selects highest number in a list
(define (highest args)
    (define (helper args out)
        (cond ((null? args) out)
              ((list? (car args))
               (let ((subhighest (highest (car args))))
                    (if (> subhighest out)
                        (helper (cdr args) subhighest)
                        (helper (cdr args) out))))
              ((and (number? (car args))
                    (> (car args) out)) (helper (cdr args) (car args)))
              (else (helper (cdr args) out))))
    (helper args 0))

; selects first n elements in a list
(define (firstn n args)
    (if (= n 1)
        (list (car args))
        (append (list (car args)) (firstn (- n 1) (cdr args)))))

; deep map for numbers
(define (deep-map proc items)
    (map (lambda (item)
            (if (number? item)
                (proc item)
                (deep-map proc item))) items))


(define (regroup pattern)
    (define (extend-pattern pattern items)
        (let ((new-pattern (append pattern (deep-map (lambda (x) (+ x (highest pattern))) pattern))))
            (if (> (highest new-pattern) (count items))
                pattern
                (extend-pattern new-pattern items))))

    (define (builder items pattern)
        (define (builder-helper sub-pattern)
            (cond ((null? sub-pattern) '())
                    ((list? sub-pattern) (builder items sub-pattern))
                    (else (selectn sub-pattern items))))
        (if (eq? '... (last pattern))
            (map builder-helper (extend-pattern (bl pattern) items))
            (map builder-helper pattern)))

    (lambda (items)
        (builder items pattern)))