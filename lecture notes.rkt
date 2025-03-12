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


;3/10
(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq)
                  (accumulate op init (cdr seq))))))
(define (op x y)
  (if (pred x)
      (cons x y)
      y))
;Use map and accumulate to add 1 to a list of integers
;@ operator -> (_ @ _)
;  :^)
;/(.|.)\
;   |
;  / \

;inductive definitions enable structural induction by providing a definition of proper
;componenets

; for a structural induction, we a ssume that the result (whatever it is)
;is true for proper components and then argue that it holds for the entire assemblage
;of those components

;talking about some constructer shit no clue whats going on, review notes
;ex:
(define @-exp? (lambda (e)
                 (eq? (operator e) '@)))

;recursive descent, syntax directed evaluation

;Self study notes
;Tree Induction: Induct on the car cdr structure of the tree
;IH: assume the result for both car and cdr
;IS: Argue the program does the right thing with the car and cdr