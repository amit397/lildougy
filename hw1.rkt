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

(display "Question 2")
(newline)
(display (myAnd #t #t))
(newline)
(display (myOr #t #f))
(newline)
(display (myNot #f))
(newline)

; QUESTION 3
(define findLen
  (lambda (x)
    (if (< x 10)
        1
        (+ 1 (findLen (quotient x 10))))))

(define sumOfSquares
  (lambda (a b c)
    (define x (findLen a))
    (define y (findLen b))
    (define z (findLen c)) 
    (if (and (< z x) (< z y))
        (+ (* a a) (* b b))
        (if (and (< y x) (< y z))
            (+ (* a a) (* c c))
            (+ (* c c) (* b b))))))

(display "Question 3")
(newline)
(display (sumOfSquares 10 1 11))
(newline)


;Question 4
(define initArr (list 7 6 4 1 2))
(define removeElem
      (lambda (targ lst)
        (apply list
               (map (lambda (x) (if (eqv? x targ) '99999999999 x)) lst))))
(define firstVal (apply min initArr))
(define firstArr (removeElem firstVal initArr))

(define secondVal (apply min firstArr))
(define secondArr (removeElem secondVal firstArr))

(define thirdVal (apply min secondArr))
(define thirdArr (removeElem thirdVal secondArr))

(define fourthVal (apply min thirdArr))
(define fourthArr (removeElem fourthVal thirdArr))

(define fifthVal (apply min fourthArr))

(list firstVal secondVal thirdVal fourthVal fifthVal) 
    
        
    
    
