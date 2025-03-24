;midterm prac 2

;Iterative Proof Structure
;Preconditon
;Guess Invariant
;Basis
;Postcondition
;Weak Enough Test
;Strong Enough Test
;Preservability
;Terminating Condition


;sortNum-Asc? proof
;Precondition: The function has two inputs: an integer n and a integer prevDig that keeps track of the prevDig
;in the integer to check to see if the integer is properly sorted. n and prevDig must be non-negative integers
;Basis: if n is 0 that must mean we have gone past the last digit and checked all previous digits (which according to the guess invariant is already in ascending order) so we can return #t. If we detect that the current digit
;we are on is greater than the prevDigit then we can return #f because it is not in ascending order. If neither of those are the case we can run the next iteration of the function by inputting the quotient of n and 10 (thus removing
;the current digit) and inputting remainder n 10 into the prevDig (thus inputting our current digit into prevDig for the next iteration).
;Guess-Invariant: All digits prior to the current one must be sorted in ascending order i.e. -> 12345 if we are at 2: 345 must be in ascending order
;Postcondition: return either #t if it is in ascending order and #f if it isn't.

;Weak Enough Test: During the first call prevDig is specified to 10 while the current digit is the last number in the integer, since any singular digit is guaranteed to be less than 10
;the invariant holds true the function keeps iterating through the integer
;Strong Enough Test: During the last call of the function the first digit in the integer is compared to the second digit in the integer and will return false if it finds the first digit is greater than the second digit,
;if not he function will call itself one more time to verify whether this is the first digit in the integer (by dividing by 10) at which point it will return #t
;Preservability: At any point in the intermediate calls if it is identified the current digit is greater than the prior digit then #f will be returned, if not the function will move onto the enxt call by going to the next digit,
;and using the current digit as the prevDig in the next call
;Terminating Condition: The function will verify that the first digit in the integer follows ascending order then divide the current number n (which should just be the first digit in the integer) by 10 to yield 0
;which means the entire integer has been checked and will return #t