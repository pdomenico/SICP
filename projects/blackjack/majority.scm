(load "stop-at-17")
(load "dealer-sensitive")
(load "valentine")

(define (majority s1 s2 s3)
    (lambda (hand dealer-card)
        (let ((s1-result (s1 hand dealer-card))
              (s2-result (s2 hand dealer-card))
              (s3-result (s3 hand dealer-card)))
            (or (and s1-result s2-result)
                (and s1-result s3-result)
                (and s2-result s3-result)))))

(define combined-strategy (majority stop-at-17 dealer-sensitive valentine))