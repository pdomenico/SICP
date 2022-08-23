; PROBLEM 2

(define datum car)
(define l-branch cadr)
(define r-branch cddr)
(define (make-tree datum l-b r-b) (cons datum (cons l-b r-b)))

(define (all-smaller? tree n)
    (cond ((null? tree) #t)
          ((>= (datum tree) n) #f)
          (else (and (all-smaller? (l-branch tree) n) (all-smaller? (r-branch tree) n)))))

(define (all-larger? tree n)
    (cond ((null? tree) #t)
          ((<= (datum tree) n) #f)
          (else (and (all-larger? (l-branch tree) n) (all-larger? (r-branch tree) n)))))

(define (bst? tree)
    (and (all-smaller? (l-branch tree) (datum tree))
         (all-larger? (r-branch tree ) (datum tree))))

; PROBLEM 3
(define make-tree cons)
(define datum car)
(define children cdr)

(define (greater items)
    (if (= 1 (length items))
        (car items)
        (let ((rest (greater (cdr items))))
            (if (> (car items) rest)
                (car items)
                rest))))


(define (fanout tree) (length (children tree)))

(define (max-fanout tree)
    (if (= 0 (fanout tree))
        0
        (greater (append (list (fanout tree)) (map max-fanout (children tree))))))

; test procedures
(define (make-empty-tree)
    (make-tree 1 '()))

(define (add-child tree child)
    (make-tree (datum tree) (append (children tree) (list child))))

(define (n-children-tree n)
    (if (= n 0)
        (make-tree 1 '())
        (add-child (n-children-tree (- n 1)) (make-tree 1 '()))))