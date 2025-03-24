;Create an algorithm that returns the permutations of a list of integers (1 2 3) -> (1 3 2) (2 1 3) (2 3 1) ...

;D and Q Design Idea: Create a recursive algorithm that selects an element of the list and specifies its position in the permutation then rearranges the position of the remaining elements of the list.
;We will create a function called permutation that will utilize map to place the first element in a specific position then a nested map calling the function again without the first element to place the
;remaining elements in different positions. For this we will need a helper function called remove which will remove the specified element from a list as to not repeat elements in the permutation.
;The base case for the permutation function will be when the input list is empty (all elements have been removed), meaning a permutation is created where each element is specified to a position, and return an empty list

;Remove helper function: constructs a new list where lst has the element x removed on each call of the 
(define (remove x lst)
  (cond ((null? lst) '())
        ((equal? x (car lst)) (remove x (cdr lst)))
        (else (cons (car lst) (remove x (cdr lst))))))

;remove test
(remove 3 '(1 2 3 4 5)) ; -> (1 2 4 5)

;Permutation main function
(define (permutation lst)
  (if (null? lst)
      '(()) ;Base case since the permutation of an empty list is a list containing an empty list
      (apply append ;Flattens list so only the elements appear 
             (map (lambda (x)
                     (map (lambda (y) (cons x y))
                          (permutation (remove x lst)))) lst))))
(permutation '(1 2 3)) ; -> ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
(permutation '()) ; -> (())
(permutation '(1 2 3 4 5))
(permutation '(a b c))
(permutation '((1 2 3) (2 3 4 5) 4))

;Proof: permutation function
;Precondition: The function takes one input lst which must be a valid list
;Base Case: If a list with no elements is input into the function then it will return a list with an empty list inside
;IH: Assume permutation works for a length of list k
;IS: Prove that permutation works for a length of list k+1
;During the kth iteration the main permutation function will detect the input list is not yet empty and move to the map function where it will construct a permutation list with the current kth element
;and cons it with the output of the next call (k+1th call) of permutation with the kth element removed from lst. During the k+1th call it will detect that it is at the end of the list and return the permutation
;of an empty list (the base case) which will get cons'd with the kth call and the k-1th call and so on.
;Postcondition: Return a list of lists that contains all permutations and where each list represents a distinct permutation of the input list.