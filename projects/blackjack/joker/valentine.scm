(load "best-total")
(load "stop-at")

(define (find-suit cards suit)
    (cond ((empty? cards) #f)
          ((equal? suit (last (first cards))) #t)
          (else (find-suit (bf cards) suit))))

(define (valentine hand dealer-card)
    (if (find-suit hand 'H)
        ((stop-at 19) hand dealer-card)
        ((stop-at 17) hand dealer-card)))