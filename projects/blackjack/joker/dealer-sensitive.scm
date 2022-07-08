(load "best-total")

(define (dealer-sensitive customer-hand dealer-card)
    (define (is-in-set? card set)
        (cond ((empty? set) #f)
                ((= (card-to-n card) (first set)) #t)
                (else (is-in-set? card (bf set)))))
    
    (let ((score (best-total customer-hand))
          (first-set '(7 8 9 10 11))
          (second-set '(2 3 4 5 6)))
        (or (and (is-in-set? dealer-card first-set) (< score 17))
                (and (is-in-set? dealer-card second-set) (< score 12)))))