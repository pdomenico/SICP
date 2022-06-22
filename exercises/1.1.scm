; Exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))


(define (improve guess x)
  (average guess (/ x guess)))


(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess improved-guess)
  (if (> guess improved-guess)
      (< (/ guess improved-guess) 1.001)
      (< (/ improved-guess guess) 1.001)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


; Exercise 1.8
(define (betterCubeRootApprox cube rootGuess)
  (/ (+ 
      (/ cube (* rootGuess rootGuess))
      (* rootGuess 2))
   3))

(define (cubeRootIter x guess)
  (if (good-enough? guess (betterCubeRootApprox x guess))
      guess
      (cubeRootIter x (betterCubeRootApprox x guess))))

(define (cubeRoot x)
  (cubeRootIter x 1.0))

