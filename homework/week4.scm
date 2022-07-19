(define (substitute l old-word new-word)
    (cond ((null? l) '())
          ((list? (car l))
           (append (list (substitute (car l) old-word new-word))
                   (substitute (cdr l) old-word new-word)))
          ((equal? (car l) old-word)
           (append (list new-word) (substitute (cdr l) old-word new-word)))
          (else (append (list (car l)) (substitute (cdr l) old-word new-word)))))