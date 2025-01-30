; QUESTION 2
(define myAnd
  (lambda (x y)
    (if (eqv? #t x)
        (if (eqv? #t y)
            #t
            #f)
        #f)))
(define myOr
  (lambda (x y)
    (if (eqv? #t x)
        #t
        (if (eqv? #t y)
            #t
            #f))))
(define myNot
  (lambda (x)
    (if (eqv? #t x)
        #f
        #t)))

(display (myAnd #t #t))
(newline)
(display (myOr #t #f))
(newline)
(display (myNot #f))
(newline)

; QUESTION 3
(define findLen
  (lambda (x)
    (if (= x 1)
        1
        (+ 1 (findLen (quotient x 10))))))

(define sumOfSquares
  (lambda (a b c)
    (define (findLen a) x)
    (define (findLen b) y)
    (define (findLen c) z) 
    (if (< z x) ; xy xz yz xz

        (if (< z y)
            (+ (* x x) (* y y))
            (+ (* x x) (* z z)))

        (if (< x y) ;x < z
            (+ (* y y) (* z z))
            (+ (* x x) (* z z))))))

(display (sumOfSquares 100 1 22)) 
