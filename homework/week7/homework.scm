(define-class (random-generator range)
    (instance-vars (count 0))
    (method (number) (set! count (+ 1 count)) (random range)))

(define-class (coke-machine max-cokes price)
    (instance-vars (credit 0) (cokes 0))
    (method (fill x)
        (if (> (+ x cokes) max-cokes)
            (error "You're trying to fill too many cokes!")
            (set! cokes (+ cokes x))))
    (method (deposit x) (set! credit (+ credit x)))
    (method (coke)
        (if (< credit price)
            (error "NOT ENOUGH MONEY")
            (begin (set! cokes (- cokes 1)) (set! credit (- credit price)) credit))))

; Deck of cards

(define (nth n items)
    (if (= n 0)
        (car items)
        (nth (- n 1) (cdr items))))

(define (remove item set)
    (define (helper prev-set post-set)
        (cond ((empty? post-set) prev-set)
              ((eq? item (car post-set)) (append prev-set (cdr post-set)))
              (else (helper (append prev-set (list (car post-set))) (cdr post-set)))))
    (helper '() set))

(define (second x) (first (bf x)))

(define ordered-deck '(AH 2H 3H 4H 5H 6H 7H 8H 9H 10H JH QH KH
                       AS 2S 3S 4S 5S 6S 7S 8S 9S 10S JS QS KS
                       AD 2D 3D 4D 5D 6D 7D 8D 9D 10D JD QD KD
                       AC 2C 3C 4C 5C 6C 7C 8C 9C 10C JC QC KC))

(define (shuffle deck)
    (if (null? deck)
    '()
    (let ((card (nth (random (length deck)) deck)))
        (cons card (shuffle (remove card deck))))))

(define-class (deck)
    (instance-vars (cards (shuffle ordered-deck)))
    (method (deal)
        (if (empty? cards)
            '()
            (let ((card (car cards)))
                (begin (set! cards (remove card cards))
                       card))))
    (method (empty?) (empty? cards)))

(define-class (miss-manners obj)
    (method (please message arg) (ask obj message arg)))