; Exercise 3.3 and 3.4
(define (make-account balance password)
  (define wrong-psw 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) (error "THE COPS ARE COMING"))
  (define (dispatch psw m)
    (if (eq? psw password)
        (begin (set! wrong-psw 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT" m))))
        (begin (set! wrong-psw (+ 1 wrong-psw))
               (if (> wrong-psw 6)
                   (call-the-cops)
                   (error "Incorrect password")))))
  dispatch)

; Exercise 3.7
(define (make-joint account original-psw new-psw)
    (lambda (psw message)
        (if (eq? psw new-psw)
            (account original-psw message)
            (error "Incorrect password!"))))

; Exercise 3.8
(define f (let ((glob 0))
            (lambda (x)
              (begin (set! glob (+ 1 glob))
                     (if (= 0 (modulo glob 2))
                         0
                         x)))))
