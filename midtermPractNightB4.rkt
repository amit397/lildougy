(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
             (map (lambda (x)
                    (map (lambda (y)
                           (cons x y))
                           (permutations (remove x lst)))) lst))))

(define (remove x lst)
  (cond ((null? lst) '())
        ((eq? (car lst) x) (remove x (cdr lst)))
        (else (cons (car lst) (remove x (cdr lst))))))
(remove 3 '(1 2 3 4 5))

(permutations '(1 2 3))

(define (bfs q n)
  (if (null? q)
      #f
      (let ((node (car q))
            (rest (cdr q)))
        (if (eq? node n)
            #t
            (if (pair? node)
                (let ((left (cadr node))
                      (right (caddr node)))
                  (bfs (append rest (list left right)) n))
                (bfs rest n))))))
(bfs '(1 (2 3 1) (4 5 6)) 6)


;subsets of a set of integers
;Design Idea: We will compute the subset of a list by computing the subsets of the same list excluding a specific element and then adding back in the specifc element to each distinct subset that is computed and returning both by
;appending them together.
;So in the example list of (1 2 3) to find the subsets of (1 2 3) we will compute the subsets of (2 3) then (3) then the () then add each excluded element back into a copy of the computed subsets. So after we reach the () we will
;add 3 into a copy of the () computing the subsets of (3) as (() (3)) then we will return this to (2 3) and add 2 to a copy of the computed subsets of (3) and append them together -> (2 3) and (2) and append it to (() (3))
;-> (() (3) (2) (2 3)) then we go back up to (1 2 3) and do the same.

;Basic D and Q: Compute the subsets of the list excluding the car then add the car in copies of the computed subsets and append them together.

(define (subsets lst)
  (if (null? lst)
      '(())  ; Base case: only the empty set
      (let* ((rest (subsets (cdr lst)))
             (with-first (map (lambda (s) (cons (car lst) s)) rest)))
        (append rest with-first))))
(subsets '(1 2 3))

;IH: Assume that this works for a list of length k
;IS: prove it works for a list of length k + 1
;When the length of the list is k+1 during the kth iteration of the function it will rely on the computation of the k+1th iteration which would be the last element/k+1th element of the input list's subset calculation.
;During the k+1th iteration the function will attempt to find the subsets of the next element, which would be the empty list, which would yield just the empty list. Then it would take a copy of the subsets of the
;empty list and append to them the k+1th element and return them along with the subsets of the empty list to the kth call.

;termination cond: The function will terminate once it reaches the end of the list and it computes the subset of the empty list. 

(define (iter-subset lists)
  (define (iter-subsetHELP lists interlist)
    (cond ((null? lists) (list interlist))
          (else (append (iter-subsetHELP (cdr lists) (cons (car lists) interlist))
                        (iter-subsetHELP (cdr lists) interlist)))))
  (iter-subsetHELP lists '()))
(iter-subset (list 1 2 3))

(define (nums-satisfy n func)
  (cond ((= n 0) 0)
        ((func (remainder n 10)) (+ 1 (nums-satisfy (quotient n 10) func)))
        (else (nums-satisfy (quotient n 10) func))))

(define (iter-nums-satisfy n func res)
  (cond ((= n 0) res)
        ((func (remainder n 10)) (iter-nums-satisfy (quotient n 10) func (+ res 1)))
        (else (iter-nums-satisfy (quotient n 10) func res))))

(nums-satisfy 1245678 odd?)
(iter-nums-satisfy 1245678 odd? 0)









(define (quickSort number listComparison list1 list2 list3)
  (cond ((null? listComparison) (append (quickSortList list1) list2 (quickSortList list3))) ;; Recursively sort partitions
        ((> (car listComparison) number) 
         (quickSort number (cdr listComparison) list1 list2 (cons (car listComparison) list3)))
        ((< (car listComparison) number) 
         (quickSort number (cdr listComparison) (cons (car listComparison) list1) list2 list3))
        (else 
         (quickSort number (cdr listComparison) list1 (cons (car listComparison) list2) list3))))

(define (quickSortList lst)
  (if (null? lst)
      '()
      (let* ((pivot (car lst)))
        (quickSort pivot (cdr lst) '() (list pivot) '())))) ;; Choose the first element as pivot and sort

(define (quickSorting listing)
  (quickSortList listing))
(quickSorting (list 57 1 2 3 4 3 2 1 32 6 7 8 54 3 3 4 5 6))





(define (replacenth tree a b n)
  (cond
    ((null? tree) tree);; if tree is null, just return null
    ;; if tree is a atom, if n = 1 and tree == a, replace with b, else just return tree
    ((not (pair? tree)) (cond
                          ((and (= n 1) (equal? tree a)) b)
                          (else tree)))
    (else
     (let ((car-count (countHz (car tree) a)))
       (cond 
        ;; if n is greater than car count, we need to replace it in cdr
        ((> n car-count) (cons (car tree) (replacenth (cdr tree) a b (- n car-count))))
        (else
         (cons (replacenth (car tree) a b n) (cdr tree)))))))) 

