#lang racket #| CSC324 Fall 2020: Exercise 2 |#
#|

|#
;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide apply-fns make-counter apply-fns sum-map-both sum-map-both-helper calculate)

(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------
; * Task 1: num-pred *
;-------------------------------------------------------------------------------

#|
(num-pred pred lst)
  pred: A function that takes one argument and returns a boolean
  lst: A list of items.

  Returns the number of items in the lst for which (pred item) returns True
|#

(define (num-pred-helper pred lst acc)
  (
   if ( = (length lst) 0)
     acc
     (if ( pred (first lst))
         
     (num-pred-helper pred (rest lst) (+ acc 1))
     (num-pred-helper pred (rest lst) acc))
    
   )

   


  )

(define (num-pred pred lst)
  (
   num-pred-helper pred lst 0
   )
  )


;-------------------------------------------------------------------------------
; * Task 2: More Higher-Order Functions * 
;-------------------------------------------------------------------------------

#|
(make-counter pred)
  pred: A function that takes one argument and returns a boolean

  Returns a function that takes a list and returns the number of elements
  in the list satisfying that predicate.
|#



(define (make-counter pred)
  ( lambda (lst)
    (num-pred pred lst))


   )

; Uncomment and define these using `make-counter`.
;(define num-evens-1 (void))
;(define num-many-evens-1 (void))

(module+ test
  (test-equal? "empty list"
               ((make-counter even?) '())
               0)
    (test-equal? "normal list"
               ((make-counter even?) '(1 2 3  4))
               2)
    (test-equal? "odd list"
               ((make-counter even?) '(1 1 1))
               0)
    
)


#|
(apply-fns fns arg)
  fns: A list of unary functions.
  arg: A value.

  Returns a list of the results of applying each function in `fns` to `arg`.
|#
(define (apply-fns-helper fns arg acc)

(
 if (= (length fns) 0)
    acc
    (apply-fns-helper (rest fns ) arg (append acc (list((first fns) arg))) )
 )
  )

(define (apply-fns fns arg)
  ; TODO: replace the (void) with a proper function body.
  (

      apply-fns-helper fns arg '()
   
  )
)
(module+ test
  (test-equal? "(apply-fns (list integer? null?) 4)"  ; Test label
               (apply-fns (list integer? null?) 4)   ; Actual value
               (list #t #f))

   (test-equal? "(apply-fns (list integer? null?) 4)"  ; Test label
               (apply-fns (list even? null?) 1)   ; Actual value
               (list #f #f)) ; Expected value
  ; TODO: write more tests
)

#|
(sum-map-both fn lst1 lst2)
  fn: A function that takes two arguments and returns a number.
  lst1: A list
  lst2: Another list of equal length

  Precondition: (equal? (length lst1) (length lst2))
  You do not need to check the pre-condition. We will only be testing
  with tests that satisfy the pre-condition.

  Returns the sum of the `fn` applied to each of the pairs of values
  in lst1 and lst2. We would like this function to be *tail-recursive*,
  so it should call the helper function `sum-map-both-helper` with
  an appropriate initial accumulator value.
|#



(define (sum-map-both fn lst1 lst2)
  (
   if (equal? fn null)
      0
      (sum-map-both-helper fn lst1 lst2 0)

   )) 
  ; (sum-map-both-helper fn lst1 lst2 TODO))


#|
(sum-map-both-helper fn lst1 lst2 acc)
  fn: A function that takes 
  lst1: A list
  lst2: Another list of equal length
  acc: A number describing the accumulated values so far

  Precondition: (equal? (length lst1) (length lst2))
  You do not need to check the pre-condition. We will only be testing
  with tests that satisfy the pre-condition.

  Helper function for sum-map-both-helper. We *will* be testing this
  helper function.
|#
(define (sum-map-both-helper fn lst1 lst2 acc)
  (if ( = (length lst1) 0 )
  acc
  ( sum-map-both-helper fn (rest lst1) (rest lst2) (+ acc  (fn (first lst1) (first lst2))))
  )

  )


(module+ test
  (test-equal? "(sum-map-both - '(5 10 7) '(2 6 5))" ; Test label
               (sum-map-both - '(5 10 7) '(2 6 5))   ; Actual value
               9)  ; Expected value = (5 - 2) + (10 - 6) + (7 - 5)

  (test-equal? "(sum-map-both + '(1 1 1 ) '(1 1 1))" ; Test label
               (sum-map-both + '(1 1 1 ) '(1 1 1 ))   ; Actual value
               6)  ; Expected value = 6
   (test-equal? "(sum-map-both * '(1 1 1 ) '(1 1 1))" ; Test label
               (sum-map-both * '(1 1 1 ) '(1 1 1 ))   ; Actual value
               3)  ; Expected value = 3
  ; TODO: write more tests
)

;-------------------------------------------------------------------------------
; * Task 4: Calculator *
;-------------------------------------------------------------------------------

#|
(calculate expr)
  expr: An expression generated by the Binary Arithmetic Expression Grammar
        described in the handout.

  Return the numerical value of the expression
|#
(define/match (calculate expr)
  
  
  [((list '+ x y)) ( + (calculate x) (calculate y))]
  [((list '- x y)) ( - (calculate x) (calculate y))]
  [((list '* x y)) ( * (calculate x) (calculate y))]
  [((list '/ x y)) ( / (calculate x) (calculate y))]
  [(x) x]
  
  )

(module+ test
  (require rackunit)
  (test-equal? "calculate: +"
               (calculate '(+ 2 3)) ;'(+ 2 3) is the same as (list '+ 2 3)
               5)
  (test-equal? "calculate: /"
               (calculate '(/ (+ 2 6) 2))
               4))

