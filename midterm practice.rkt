;Design Idea: We have a function that iteratively goes through each digit starting from the end and checking if the previous digit is greater than/less than the current one and returning false depending on the sort
;predicate that we are given. If we reach the end before we encounter such an occurence then we would return true. This will be within a wrapper function that checks whether or not the user wants it in ascending order
;or descending order and running the appropriate helper function with the given integer. It will also feed the appropriate helper function the absolute value of the function so negatives are not an issue.

;Guess Invariant: All the previous digits from the current digit are in proper ascending or descending order.

(define (sortNum? n asc?)
  (define (sortNum-Asc? n prevDig)
    (cond ((= n 0) #t)
          ((> (remainder n 10) prevDig) #f)
          (else (sortNum-Asc? (quotient n 10) (remainder n 10)))))

  (define (sortNum-Desc? n prevDig)
    (cond ((= n 0) #t)
          ((< (remainder n 10) prevDig) #f)
          (else (sortNum-Desc? (quotient n 10) (remainder n 10)))))
    
  (if (eq? asc? #t)
      (sortNum-Asc? (abs n) 10)
      (sortNum-Desc? (abs n) 0)))

(Display "Iterative Function Test")
(newline)
(sortNum? 12345 #t) ;-> #t
(sortNum? 12345 #f) ;-> #f
(sortNum? 54321 #f) ;-> #t
(sortNum? 54321 #t) ;-> #f
(sortNum? 0 #t) ;-> #t
(sortNum? 0 #f) ;-> #t
(sortNum? 1 #t) ;-> #t
(sortNum? 1 #f) ;-> #t
(sortNum? -12345 #t) ;-> #t
(sortNum? -12345 #f) ;-> #f

;sortNum?
;Precondition: This iterative function takes two inputs: n which is a integer, and asc? which is a boolean that represents whether the user wants the function to check for the integer to be sorted in a ascending or descending order.
;Basis: If the user wants to check if the integer is in ascending order then run sortNum-Asc? With the absolute value n and a prevDigit 10 (since no single digit can be greater than or equal to 10 it ensures the first iteration
;functions as expected) will be exectured. If the user wants to check descending order then sortNum-Desc? with the absolute value n and a prevDigit of 0 (since no single digit can be lower than 0)
;Postcondition: Return true or false depending on whether or not the integer is sorted in ascending or descending order

;sortNum-Asc? proof
;Precondition: The function has two inputs: an integer n and a integer prevDig that keeps track of the prevDig
;in the integer to check to see if the integer is properly sorted. n and prevDig must be non-negative integers
;Basis: if n is 0 that must mean we have gone past the last digit and checked all previous digits (which according to the guess invariant is already in ascending order) so we can return #t. If we detect that the current digit
;we are on is greater than the prevDigit then we can return #f because it is not in ascending order. If neither of those are the case we can run the next iteration of the function by inputting the quotient of n and 10 (thus removing
;the current digit) and inputting remainder n 10 into the prevDig (thus inputting our current digit into prevDig for the next iteration).
;Guess-Invariant: All digits prior to the current one must be sorted in ascending order i.e. -> 12345 if we are at 2: 345 must be in ascending order
;Postcondition: return either #t if it is in ascending order and #f if it isn't.


;sortNum-Desc? proof -> it is the exact same as sortNum-Asc? but checking if numbers are in descending order. I will rewrite it below with said changes.
;Precondition: The function has two inputs: an integer n and a integer prevDig that keeps track of the prevDig
;in the integer to check to see if the integer is properly sorted. n and prevDig must be non-negative integers
;Basis: if n is 0 that must mean we have gone past the last digit and checked all previous digits (which according to the guess invariant is already in descending order) so we can return #t. If we detect that the current digit
;we are on is less than the prevDigit then we can return #f because it is not in descending order. If neither of those are the case we can run the next iteration of the function by inputting the quotient of n and 10 (thus removing
;the current digit) and inputting remainder n 10 into the prevDig (thus inputting our current digit into prevDig for the next iteration).
;Guess-Invariant: All digits prior to the current one must be sorted in ascending order i.e. -> 54321 if we are at 4: 321 must be in descending order
;Postcondition: return either #t if it is in descending order and #f if it isn't.



;RECURSIVE FUNCTION
;Design Idea: utilize let to assign the value of the recursive calls of sortNum-Asc? and sortNum-Desc? to a variable called res. Then at the function evaluate res and return the proper value to the wrapper.

(define (sortNumRec? n asc?)
  (define (sortNum-Asc-Rec? n prevDig)
    (cond ((= n 0) #t)
          ((> (remainder n 10) prevDig) #f)
          (else (let ((res (sortNum-Asc-Rec? (quotient n 10) (remainder n 10))))
                  (if (eq? res #t)
                    #t
                    #f)))))

  (define (sortNum-Desc-Rec? n prevDig)
    (cond ((= n 0) #t)
          ((< (remainder n 10) prevDig) #f)
          (else (let ((res (sortNum-Desc-Rec? (quotient n 10) (remainder n 10))))
                  (if (eq? res #t)
                    #t
                    #f)))))
    
  (if (eq? asc? #t)
      (sortNum-Asc-Rec? (abs n) 10)
      (sortNum-Desc-Rec? (abs n) 0)))

(newline)
(display "Recursive Function Tests")
(newline)
(sortNumRec? 12345 #t) ;-> #t
(sortNumRec? 12345 #f) ;-> #f
(sortNumRec? 54321 #f) ;-> #t
(sortNumRec? 54321 #t) ;-> #f
(sortNumRec? 0 #t) ;-> #t
(sortNumRec? 0 #f) ;-> #t
(sortNumRec? 1 #t) ;-> #t
(sortNumRec? 1 #f) ;-> #t
(sortNumRec? -12345 #t) ;-> #t
(sortNumRec? -12345 #f) ;-> #f


;sortNumRec?
;Precondition: This recursive function takes two inputs: n which is a integer, and asc? which is a boolean that represents whether the user wants the function to check for the integer to be sorted in a ascending or descending order.
;Basis: If the user wants to check if the integer is in ascending order then run sortNum-Asc? With the absolute value n and a prevDigit 10 (since no single digit can be greater than or equal to 10 it ensures the first iteration
;functions as expected) will be exectured. If the user wants to check descending order then sortNum-Desc? with the absolute value n and a prevDigit of 0 (since no single digit can be lower than 0)
;Postcondition: Return true or false depending on whether or not the integer is sorted in ascending or descending order.

;;sortNum-Asc? proof
;Precondition: The function has two inputs: an integer n and a integer prevDig that keeps track of the prevDig
;in the integer to check to see if the integer is properly sorted. n and prevDig must be non-negative integers
;Basis: if n is 0 that must mean we have gone past the last digit and checked all previous digits (which according to the guess invariant is already in ascending order) so we can return #t. If we detect that the current digit
;we are on is greater than the prevDigit then we can return #f because it is not in ascending order. If neither of those are the case we can recursively call the function by deferring the
;assignment of the value of res with whatever the next call of the sortNum-Asc? returns is by inputting the quotient of n and 10 (thus removing
;the current digit) and inputting remainder n 10 into the prevDig (thus inputting our current digit into prevDig for the next call).
;IH: Assume that this works for some integer of length n
;IS: Prove that it works for an integer of length n+1
;During the assignment of res to the boolean for the nth call we call the function sortNum-Asc? again with the n+1th call where we check the next digit of the integer and the current digit to validate its compliance to our sort
;predicate. Since the n+1th call is our last digit we will reach the base case then return either #t or #f at which point this boolean will be returned for assignment to res in the nth call at which point the conditional underneath
;the let statement will be executed and a boolean will be returned to n-1th call where the n-1th res is assigned to that boolean and the same if conditional is run and returns a boolean to the n-2th call and so on until the first
;call at which point a boolean will be returned. Thus proving that the function works for the n+1th call.
;Postcondition: return either #t if it is in ascending order and #f if it isn't.