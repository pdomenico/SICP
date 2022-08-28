(define Dormitory (instantiate place 'Dormitory))
(define Domenico (instantiate person 'Domenico Dormitory))
(can-go Dormitory 'east Soda)
(define Kirin (instantiate place 'Kirin))
(can-go Dormitory 'north Kirin)
(can-go Kirin 'south Dormitory)
(define potstickers (instantiate thing 'potstickers))
(ask Kirin 'appear potstickers)

; Domenico goes to Kirin and takes potstickers
(ask Domenico 'go 'north)
(ask Domenico 'take potstickers)
; Domenico goes to brian and lets him take the potstickers
(display "yes") (newline)
(ask Domenico 'go 'south)
(ask Domenico 'go 'east)
(ask Domenico 'go 'up)
(ask Domenico 'go 'west)
(ask Domenico 'lose potstickers)
(ask Brian 'take potstickers)
; Go back to the lab
(ask Domenico 'go 'east)
(ask Domenico 'go 'down)
(ask Domenico 'go 'south)
(ask Domenico 'go 'south)

; WHEREIS procedure
(define (whereis person)
  (ask (ask person 'place) 'name))

; Owner procedure
(define (owner thing)
  (let ((o (ask thing 'possessor)))
    (if (null? o)
        (display "nobody")
        (ask o 'name))))