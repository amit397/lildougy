(define (dec x)
  (- x 1))

(define (inc x)
  (+ x 1))



(define (myAdd x y)
  (cond ((zero? y) x)
        ((zero? x) y)
        (else (if (> 0 x)
                  (myAdd (inc x) (dec y))
                  (myAdd (dec x) (inc y))))))
(myAdd 0 -1)
                  
;precondition of myAdd: x and y are integers, inc() and dec() add and subtract 1 respectfully
;basis: if either input x or y are 0 return the variable that isn't

;inductive hypothesis: set y=k and assume that the program works for myAdd(x, k)
;inductive step: prove the program would work for myAdd(x, k+1)
;myAdd(x, k + 1) = x + (k + 1) = x + k + 1
;this function can be expressed as myAdd(inc(x), dec(k+1))
;since myAdd(inc(x), dec(k+1))= (x + 1) + ((k + 1) - 1) = (x + 1) + k = x + 1 + k = myAdd(x, k + 1)
;which is the same as myAdd(inc(x), k) since myAdd(inc(x), k) = (x + 1) + k = x + k + 1
;since we know myAdd(x, k) works properly and that inc(x) works properly
;myAdd(inc(x), k) must also work properly
;Thus myAdd(x, k+1) must work properly as well

;Postcondition: must return x + y

(define (insertIntoList lst item indx)
        (insertIntoListHelper lst item indx))

(define (insertIntoListHelper lst item indx)
  (cond ((= indx 0) (cons item lst))
        ((equal? lst '()) (list item))
        (else (cons (car lst) (insertIntoListHelper (cdr lst) item (- indx 1))))))

(insertIntoList '(2 3 4) 1 5)

;PreCondition: lst must be a list, item must be an object that can be put into a list in scheme and indx must be a positive integer >= 0
;Basis: If indx is 0 then we have reached where the item should be inserted into the list and the function will return a new list that will be constructed with
;item as the first element and subsequent elements being lst. If the lst inputted into the function is empty a new list with just item will be returned

;Ind Hyp: Assume that the function insertIntoListHelper functions for indx=k-1
;Ind Step: Prove that insertIntoListHelper works for indx=k
;The return value for insertIntoListHelper where indx=k is reliant on the output of insertIntoListHelper where indx=k-1
;since it constructs a new array with the car of the lst and whatever insertIntoListHelper indx=k-1 yields and since we
;assumed that insertIntoListHelper at indx=k-1 works
;Thus insertIntoListHelper at indx=k must work as well

;Postcondition: insertIntoListHelper yields a new list with item inserted at the proper indx within the lst or at the end of the lst if the indx is out of range.
;In the case that lst is an empty list it yields a new list with item as the only element




;TWO SUM

;test array: (2, 7, 11, 15) targ: 9
;Design Idea: Have three inputs, the list, the target, and a counter (to represent the index). In the function first
;check if the car of the list is the target
;if so then create a list with just the value that counter is at. If it somehow recurses to the end of the array
;and doesnt find any match for the target then
;return false. If its not the end of the array then check if it is possible to assume that the current car of
;the list is one of the elements and put inside an
;if statement the function with the cdr of the test array, target minus the car, and count incremented by one.
;If it returns true then create an array with the
;current count value and the count value that recursively calling the function would yield. If it is false then
;just recursively call the function with the cdr
;of the test array target unchanged and count incremented.

;Summary of Design Idea: Check if each value included is within the pair solution by dividing and
;conquering the computation to see if the complement of the initial value
;chosen (the complement is a value that sums with the initial to become the target) exists.
;If it does then rerun the function by deferring the construction of a new
;array which will end up containing the index of the initial value and the complement. If it doesn't then
;check the next value.

;Note on design Idea: I could introduce another variable that keeps track of the amount of values that add up to the target and limit it to 2 however it may be easier
;to confine the precondition
#|
(define (twoSum lst targ count)
  (cond  ((equal? '() lst) #f)
         ((= targ (car lst)) (list count))
        (else
              (if (twoSum (cdr lst) (- targ (car lst)) (+ count 1))
                  (cons count (twoSum (cdr lst) (- targ (car lst)) (+ count 1)))
                  (twoSum (cdr lst) targ (+ count 1))))))
(twoSum '(2 7 11 15) '9 0)
(twoSum '(2 7 11 15) '17 0)|#

;Precondition: Have an list of integers where it has
;at least a length of 2, a target that is equivalent
;to the sum of any 2 integers within that array and not
;within the array and cannot be reached with any other
;sum of integers, and count will always be 0 since
;it's used to represent the index of the first element within the array
;Basis: If the end of the list is reached without
;finding the complement of the initial value that
;was chosen whose sum would result in
;the target then return false and choose and
;different initial value. If a complement is found
;that does sum with the initial value to the target
;then construct a
;new list with the index of the complement and
;return it

;Ind Hyp: Assume that this works for an array of
;length k
;Ind Step: Prove that this function works for an
;array of length k+1

;If the solution pair is within the first k values,
;the k+1th value wouldn't matter and would return false
;if it ever recurses to that point since in that case the function
;would be picking the wrong initial value.
;If one of the values within the pair solution is the
;k+1th value the function will first find the initial
;value, then recurse to the k+1th value and
;return a new list with the index of the k+1th value
;then construct another new list with that list and
;the index of the initial value chosen.
;As a result if the function works for the kth call
;it must work for the k+1th call

;Not sure if this is needed:
;This is maintained by the invariant which expands the
;search accordingly by cdring the list and incrementing
;the count whether or not a value is chosen.

;Thus by the logic of the function the k+1th element
;would be ignored if it wasn't pertinent to the
;solution, or included if it is.

;Postcondition: A list with two indicies where
;they represent the location of the two values that
;sum to the target






;TWO SUM Rewrite

;Design Idea: Have a helper function that iteratively takes in the inputs lst, targ that outputs true or false based on whether the complement of the current number exists
;within the list. Then there will be a main function that takes the inputs lst targ and count; count is used to keep track of the indicies of the elements
;The main function goes through each element and uses the helper function to see if the complement exists, if it does start defer constructing a list with
;indicies of both values then cdr into the list until that complement is found, if not then cdr into the list and check the next value. 
(newline)
(display "TWO SUM LINE 153")
(newline)
(define (twoSumHelper lst targ)
  (cond ((null? lst) #f)
        ((= targ (car lst)) #t)
        (else (twoSumHelper (cdr lst) targ))))

(define (twoSumMain lst targ count)
  (cond ((= targ (car lst)) (list count))
        ((twoSumHelper (cdr lst) (- targ (car lst))) (cons count (twoSumMain (cdr lst) (- targ (car lst)) (+ count 1))))
        (else (twoSumMain (cdr lst) targ (+ count 1)))))

;Test [2, 7, 11, 15] targ = 13
(twoSumMain '(1 6 11 15 3) 9 0)

;Proof for twoSumMain
;Precondition: twoSumMain has three inputs: lst targ and count. lst must be a list containing only integers of at least length 2, count must be 0 since it represents
;the index of the first value, and target must be a value that is the sum of exactly two integers from lst

;Basis: If the target is found and it's not the first element, the complement of the integer that was chosen by the function is found so create a new list with the
;current index and return it

;Ind Hyp: Assume twoSumMain works for an array of length k
;Ind Step: Prove that it works for an array of length k+1
;If the solution pair exists within the first k elements, the solution pair will be found without ever reaching element k+1. Element k+1 will only be read by twoSumHelper.
;If the solution pair does include element k+1, the function will find the first element that is a part of the solution pair then recurse down to k+1 and return a new list
;containing the index of element k+1.
;Thus since the function works for the kth call it must also work for the k+1th call.

;Postcondition: Return a list containing two integers representing the two indicies of the values that sum to targ

;Proof for twoSumHelper
;Precondition: twoSumHelper has two inputs: lst and targ. lst must be a list of integers and targ must be a integer
;Basis: If the integer is not found and the end of lst is achieved or lst is empty return false

;Invariant: The car of lst is an integer and targ is also an integer
;Proof of Invariant:
;lst is guaranteed to be a lst of integers so the car of lst must also be an integer and since targ is guaranteed to be an integer according to the precondition.
;since in all of the iterative calls you are cdring into lst (a list of integers) all future cars must either be a integer or an empty list. targ remains unchanged
;between iterations.

;Postcondition: A boolean returning whether or not the targ exists within lst

(cddr '(1 2 3))

(define (twoSum l target)

  (define (helper l copy target)
    (cond
      ((null? l)'())
      ((null? copy) (helper (cdr l) (cdr (cdr l)) target))
      ((= (+ (car l) (car copy)) target) (list (car l) (car copy)))
      (else (helper l (cdr copy) target))))
  
  (helper l (cdr l) target))
(twoSum '(1 5 3 4) 8)
      