; Helper functions

(define (element? item group)
    (cond ((null? group) #f)
          ((equal? item (car group)) #t)
          (else (element? item (cdr group)))))

(define (prep-arglist args)
    (if (null? (cdr args))
        (car args)
        args))

(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items) (accumulate op init (cdr items)))))

(define (implement-word args)
    (if (null? (cdr args))
        (word (car args))
        (word (car args) (implement-word (cdr args)))))


; Read eval print loop
(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
    ((word? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? '+ fn) (accumulate + 0 args))
	((eq? '- fn) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? '* fn) (accumulate * 1 args))
	((eq? '/ fn) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
    ((eq? 'first fn) (first (prep-arglist args)))
    ((or (eq? 'bf fn) (eq? 'butfirst fn)) (bf (prep-arglist args)))
    ((eq? 'last fn) (last (prep-arglist args)))
    ((or (eq? 'butlast fn) (eq? 'bl fn)) (bl (prep-arglist args)))
    ((eq? 'word fn) (implement-word args))
	(else (error "Calc: bad operator:" fn))))