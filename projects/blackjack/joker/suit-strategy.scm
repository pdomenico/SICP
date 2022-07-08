(load "valentine")
(load "stop-at")

(define (suit-strategy suit s1 s2)
    (lambda (hand dealer-card)
            (if (find-suit hand suit)
                (s1 hand dealer-card)
                (s2 hand dealer-card))))


(define (new-valentine hand dealer-card)
    ((suit-strategy 'h (stop-at 19) (stop-at 17)) hand dealer-card))