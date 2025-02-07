 ; Return sorted list

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
