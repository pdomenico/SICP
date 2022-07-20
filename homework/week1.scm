; Homework N°1

; Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))

; Eva demonstrates the program for Alyssa:

; (new-if (= 2 3) 0 5)
; 5

; (new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))

; What happens when Alyssa attempts to use this to compute square roots? Explain.
;
; EXPLAINER:
; since new-if is an ordinary procedure, all the arguments will be evaluated and executed, this will mean that, 
; when calling the sqrt-iter function, the recursive call will always be executed and it will stuck the program in 
; an infinite loop




; HOMEWORK N2
; Write a procedure squares that takes a sentence of numbers as its argument and
; returns a sentence of the squares of the numbers

(define (squareList list)
    (define (square x)
        (* x x))

    (if (empty? list)
        '()
        (sentence (square (first list)) (squareList (bf list)))))


; 3. Write a procedure switch that takes a sentence as its argument and returns a sentence
; in which every instance of the words I or me is replaced by you, while every instance of
; you is replaced by me except at the beginning of the sentence, where it’s replaced by I.
; (Don’t worry about capitalization of letters.) Example:
; > (switch ’(You told me that I should wake you up))
; (i told you that you should wake me up)

(define (switch phrase)
    (define (checkAndReplace wrd)
        (cond ((or (equal? wrd 'me) (equal? wrd 'i) (equal? wrd 'I)) 'you)
              ((equal? wrd 'you) 'me)
              (else wrd)))

    (define (firstCheckAndReplace wrd)
        (cond ((equal? wrd 'you) 'I)
              (else wrd)))
    
    (define (subSwitch subPhrase)
        (if (empty? subPhrase)
            '()
            (sentence (checkAndReplace (first subPhrase)) (subSwitch (bf subPhrase)))))
    
    (sentence (firstCheckAndReplace (first phrase)) (subSwitch (bf phrase))))

; 4. Write a predicate ordered? that takes a sentence of numbers as its argument and
; returns a true value if the numbers are in ascending order, or a false value otherwise.

(define (ordered? numbers)
    (define (<= x y)
        (or (< x y) (= x y)))

    (cond ((empty? (bf numbers)) #t)
          ((<= (first numbers) (first (bf numbers))) (ordered? (bf numbers)))
          (else #f)))


; 5. Write a procedure ends-e that takes a sentence as its argument and returns a sentence
; containing only those words of the argument whose last letter is E:
; > (ends-e ’(please put the salami above the blue elephant))
; (please the above the blue)

(define (ends-e phrase)
    (define (wordEnds-e w)
        (equal? (last w) 'e))
    
    (cond ((empty? phrase) '())
          ((wordEnds-e (first phrase)) (sentence (first phrase) (ends-e (bf phrase))))
          (else (ends-e (bf phrase)))))


; 6. Most versions of Lisp provide and and or procedures like the ones on page 19. In
; principle there is no reason why these can’t be ordinary procedures, but some versions of
; Lisp make them special forms. Suppose, for example, we evaluate
; (or (= x 0) (= y 0) (= z 0))
; If or is an ordinary procedure, all three argument expressions will be evaluated before or
; is invoked. But if the variable x has the value 0, we know that the entire expression has
; to be true regardless of the values of y and z. A Lisp interpreter in which or is a special
; form can evaluate the arguments one by one until either a true one is found or it runs out
; of arguments.
; Your mission is to devise a test that will tell you whether Scheme’s and and or are special
; forms or ordinary functions. This is a somewhat tricky problem, but it’ll get you thinking
; about the evaluation process more deeply than you otherwise might.
; Why might it be advantageous for an interpreter to treat or as a special form and evaluate
; its arguments one at a time? Can you think of reasons why it might be advantageous to
; treat or as an ordinary function?

(define (test x)

    (if (or (= x 0) (= y 0))
        '(condition true)
        '(condition false)))

; If 0 as argument is provided, the function does not cause an error (although y is not defined),
; which means that scheme does not check for (= y 0). Otherwise, if x != 0, the function will 
; generate an error "test: variable 'y' unbound"
;
; It might be advantageous to treat or as a special for for efficiency purposes and
; it might be advantageous to treat or a a normal procedure when the program relies 
; on evaluation of every clause (example: there's a function that runs when the clause is evaluated)