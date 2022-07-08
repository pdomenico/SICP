(define (reckless strategy)
    (lambda (hand dealer-card)
        (strategy (butlast hand) dealer-card)))