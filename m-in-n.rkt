;integers n and m >= 0 and m is contained in n if the digits of m all occur in n in the same order with same multiplicity
;1 2 3 =/ 3 1 2
;1 2 2 3 =/ 3 2 1
;1 2 3 = 1 0 2 4 3 5
;m - n


;nested recursive function, checks if the last number is valid then returns the remaining

;guess invariant: m-in-n?(m, n) = (if (remainder m 10) is (remainder n 10)) then (quotient m 10) must be in (quotient n 10) 

(define (m-in-n? m n)
  (cond ((= 0 m) #t)
        ((= 0 n) #f)
        ((= (remainder m 10) (remainder n 10)) (m-in-n? (quotient m 10) (quotient n 10)))
        (else (m-in-n? m (quotient n 10)))))

(m-in-n? 123 102543)
(m-in-n? 123 13254)
(m-in-n? 1223 321)
(m-in-n? 101 1101)