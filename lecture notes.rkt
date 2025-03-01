;lecture 2/10

(define (atom? x)
  (and (not (null? x))
       (not (pair? x)))) ;pair doesn't work with lists created with cons
;an alternative would be to use list? like the function below but list? doesn't work with dotted pairs

(define (alt-atom? x)
  (not (list? x)))

;you can use (load) to load files and import functions from a different file kind of like import in python
;check documentation for more info

;want postcondition to be strong and restrictive
;referential transparency

;always start with proofs

;precond: elts of x all work for eq?

(define (contains-empty? x)
  (cond ((null? x) #f) ;induction step on this line
        ((eq? (car x) '()) #t)
        (else (contains-empty? (cdr x)))))

;weak v strong induction review before next class

;2/19 lecture

(define iter ;iterative factorial
  (lambda (count result n)
    (cond ((= count n) result)
          (else
           (iter (+ count 1)
                 (*  (+ count 1) result)
                 n)))))

;the invariant is true each time iter is called
; hence true the last time iter is called
; we can conclude from this taht the program works
;invariant is result = count!
;theta n time and constant space

;entire class was about creating invariants
;does iteration the only one with invariant
;iterative = tail recursion ? i think


;2/24
;Back of the envelope section must be my first section
;if I want to do recursion then I have to start off with how i can make the problem into
;a divide and conquer problem: find a related smaller problem that will consistently be broken
;up and solved
