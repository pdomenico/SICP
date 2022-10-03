(define stream-car car)
(define (stream-cdr s) (force (cdr s)))
(define (stream-ref stream index)
  (if (= index 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- index 1))))
(define ones (cons 1 (delay ones)))

(define (printl x) (display x) (display " "))
(define (stream-print s n)
  (if (= n 1)
      (begin (printl (stream-car s))
	     (newline))
      (begin (printl (stream-car s))
	     (stream-print (stream-cdr s) (- n 1)))))

(define (stream-map f s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (cons (f (stream-car s1) (stream-car s2))
	    (delay (stream-map f (stream-cdr s1) (stream-cdr s2))))))

(define integers
  (cons 1 (delay (stream-map + ones integers))))
