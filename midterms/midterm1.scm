; This is the programming part of the first midterm posted at: https://inst.eecs.berkeley.edu//~cs61a/reader/mt1-1.pdf

; Problem 5
(define (vowel? letter)
    (member? letter '(a e i o u)))

(define (syllables wrd)
    (define (eliminate-initial-vowels wrd)
        (cond ((empty? wrd) '())
              ((vowel? (first wrd)) (eliminate-initial-vowels (bf wrd)))
              (else wrd)))
            
    (define (helper count wrd)
        (cond ((empty? wrd) count)
              ((vowel? (first wrd)) (helper (+ 1 count) (eliminate-initial-vowels wrd)))
              (else (helper count (bf wrd)))))
    (helper 0 wrd))


; Problem 6 part a
(define (second x) (first (bf x)))

(define (shorter? a b)
    (< (count a) (count b)))

(define (in-order? f items)
    (cond ((empty? items) #t)
          ((empty? (bf items)) #t)
          ((f (first items) (second items)) (in-order? f (bf items)))
          (else #f)))


; Problem 6 part b
(define (order-checker f)
    (define (checker f items)
        (cond ((empty? items) #t)
              ((empty? (bf items)) #t)
              ((f (first items) (second items)) (checker f (bf items)))
              (else #f)))
    
    (lambda (items) (checker f items)))

; Problem 7
(define (make-time hr mn cat) (list hr mn cat))
(define hour car)
(define minute cadr)
(define category caddr)

(define (time-print-form time)
    (display (hour time))
    (display ":")
    (display (minute time))
    (display (category time))
    (newline))

(define (24-hour time)
    (if (equal? 'am (category time))
        (+ (* 100 (hour time)) (minute time))
        (+ (* 100 (+ 12 (hour time))) (minute time))))

; Rewrie constructor and selector to have 24-hour format
(define (make-time hr mn cat)
    (if (equal? 'am cat)
        (+ (* 100 hr) mn)
        (+ (* 100 (+ 12 hr)) mn)))

(define (hour time)
    (if (> time 1200)
        (- (/ (- time (modulo time 100)) 100) 12)
        (/ (- time (modulo time 100)) 100)))

(define (minute time)
    (modulo time 100))

(define (category time)
    (if (< (hour time) 12)
        'am
        'pm))