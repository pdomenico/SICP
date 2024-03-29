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

; Exercise 3.21
(define (make-queue) (cons '() '()))
(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

(define (print-queue queue) (front-ptr queue))

; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! ptr) (set! front-ptr ptr))
    (define (set-rear-ptr! ptr) (set! rear-ptr ptr))
    (define (insert element)
      (let ((new-pair (cons element '())))
        (if (empty-queue?)
            (begin (set-front-ptr! new-pair) (set-rear-ptr! new-pair))
            (begin (set-cdr! rear-ptr new-pair) (set-rear-ptr! new-pair)))))
    (define (delete)
      (if (empty-queue?) (error "Called with empty queue!"))
      (set-front-ptr! (cdr front-ptr)))
    (define (dispatch m)
      (cond ((equal? 'insert m) insert)
            ((equal? 'delete m) (delete))
            ((equal? 'front m) (car front-ptr))
            ((equal? 'rear m) (car rear-ptr))
            ((equal? 'print m) front-ptr)
            (else (error "Message not valid!"))))
    dispatch))
  
(define (add-item q item)
  ((q 'insert) item))

(define (delete-item q) (q 'delete))
(define (print-queue q) (q 'print))
(define (front-queue q) (q 'front))
(define (rear-queue q) (q 'rear))

; Exercise 3.25
(define (length ls)
  (if (null? ls) 0 (+ 1 (length (cdr ls)))))

(define (make-table)
  (let ((inside-table (list '*table*)))
    (define (insert keys value)
      (define (insert-table table keys value)
        (let ((result (assoc (car keys) (cdr table))))
          (if result
              (if (> (length (cdr keys)) 1)
                         (insert-table result (cdr keys) value)
                         (insert-value result (cadr keys) value))
              (let ((newtable (cons (car keys) '())))
                (begin (set-cdr! table (cons newtable (cdr table)))
                       (if (> (length (cdr keys)) 1)
                           (insert-table newtable (cdr keys) value)
                           (insert-value newtable (cadr keys) value)))))))
      (define (insert-value table key value)
        (let ((result (assoc key (cdr table))))
          (if result
              (set-cdr! result value)
              (set-cdr! table (cons (cons key value) (cdr table))))))
      (if (> (length keys) 1)
          (insert-table inside-table keys value)
          (insert-value inside-table (car keys) value)))
    (define (lookup keys)
      (define (inside-lookup keys table)
        (let ((result (assoc (car keys) (cdr table))))
          (if (not result)
              #f
              (if (> (length keys) 1)
                  (inside-lookup (cdr keys) result)
                  (cdr result)))))
      (inside-lookup keys inside-table))
    (define (dispatch m)
      (cond ((equal? 'put m) insert)
            ((equal? 'get m) lookup)
            ((equal? 'table m) inside-table)))
    dispatch))


(define (put table value . keys)
  ((table 'put) keys value))
(define (get table . keys)
  ((table 'get) keys))
(define (get-table table)
  (table 'table)) 


; Exercise 3.50
(define ones (cons 1 (delay ones)))

(define stream-car car)
(define (stream-cdr stream)
  (force (cdr stream)))
  

(define (print-stream stream n)
  (if (= n 1)
      (begin (display (stream-car stream))
             (newline)) 
      (begin (display (stream-car stream))
             (newline)
             (print-stream (stream-cdr stream) (- n 1)))))
        
(define (stream-map f s1 s2)
  (cons (f (stream-car s1) (stream-car s2))
        (delay (stream-map f (stream-cdr s1) (stream-cdr s2)))))

(define natural-numbers
  (cons 1 (delay (stream-map + ones natural-numbers))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons
       (apply proc (map stream-car argstreams))
       (delay (apply stream-map
                     (cons proc (map stream-cdr argstreams)))))))
; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (sum-streams s1 s2)
  (stream-map + s1 s2))

(define factorials (cons 1 (delay (mul-streams (stream-cdr natural-numbers) factorials))))

; Exercise 3.55
(define partial-sums
  (cons 1 (delay (sum-streams (stream-cdr natural-numbers) partial-sums))))

; Exercise 3.64
(define (stream-limit stream tolerance) 
  (let ((first (stream-car stream)) (second (stream-car (stream-cdr stream))))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))