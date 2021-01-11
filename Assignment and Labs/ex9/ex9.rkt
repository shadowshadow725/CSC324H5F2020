#lang racket #| CSC324 Fall 2020: Exercise 9 |#

(require "mk.rkt")
(require "numbers.rkt")
(require "evalo.rkt")

(provide reverseo changeo pbe)

;-------------------------------------------------------------------------------
; * Task 1: The relation reverseo*
;-------------------------------------------------------------------------------

#|
(reverse lst)
  lst:  A list.

  Returns a list with the elements of "lst" reversed.
  For example, (reverse '(1 2 3)) will return '(3 2 1)
  This function will not be tested. However, we recommend
  that you begin by writing this function before writing
  relation reverseo.

  At this point in the course, you should be able to write
  this function without any help or starter code. However,
  we're providing structured starter code to make the
  transition from the function to the relation more clear.
|#
(define (reverse lst)
  (cond [(eq? (length lst) 0 )  ; base case condition todo
         (list )] ; base case body todo
        [else
         (let* ([lst-first (first lst)]
                [lst-rest  (rest lst)])
           (append (reverse lst-rest)      ; recursive case todo
                   (list lst-first)))])) ; recursive case todo


#|
(reverseo lst rlst)
  lst:  A list.
  rlst: A list.

  The relation holds if rlst is the reverse of lst.
  You may use the function appendo imported from numbers.rkt.
|#
(define (reverseo lst rlst)
  (conde [(== lst '()) (== rlst '())]
         [(fresh (lst-first lst-rest rlst-last rlst-rest )
                 (== lst (cons lst-first lst-rest))
                 (appendo rlst-rest rlst-last rlst)
                 (appendo rlst-last lst-rest lst)
                 (reverseo lst-rest rlst-rest)
                 ) ]



         )) ; TODO

(module+ test
  (require rackunit)
  (test-equal? "reverseo - simple"
               (first (run 1 (x) (reverseo '(1 2 3) x)))
               '(3 2 1)))

;-------------------------------------------------------------------------------
; * Task 2: Fixing Changeo *
;-------------------------------------------------------------------------------

#|
(changeo coins total denoms)
  coins: A list of miniKanren numbers representing the list of coins
         that should make up the total value.
  total: A miniKanren number representing the total worth of the coins
  denoms: A list of miniKanren numbers representing the allowed denominations
          of the coins.

  The relation holds if `coins` is a valid way to give change with value
  `total` using coins in allowed in `denoms`.

  This current implementation produces duplicate results.
|#
(define (changeo coins total denoms)
  (conde ((== coins '())
          (zeroo total))
         ((== denoms '())
          (zeroo total))
         ((fresh (c coins^ subtotal denoms^ lost )
            (== coins (cons c coins^))
            (== denoms (cons lost denoms^))
            (=/= c lost)
            (membero c denoms)
            (pluso subtotal c total)
            (changeo coins^ subtotal denoms)))))

#|
(membero elem lst)
  elem: A term
  lst:  A list of terms

  The relation holds if `elem` is element of the list `lst`.

  This is a helper function for the `changeo` function. You may or may not
  choose to use this function in your implementation of `changeo`.
|#
(define (membero elem lst)
  (fresh (first rest)
    (== lst (cons first rest))
      (conde
        ((== first elem))
        ((membero elem rest)))))


(module+ test
  (test-equal? "changeo-simple"
               (let*
                 ([result (run 2
                               (coins)
                               (changeo coins
                                        (build-num 2)
                                        (list (build-num 5) (build-num 1))))]
                  [resultnum  (map (lambda (coins) (map toint coins))
                                   result)])
                 resultnum)
               '((1 1))))

;-------------------------------------------------------------------------------
; * Task 3: Program Synthesis *
;-------------------------------------------------------------------------------

#|
(pbe (<input> <output>) ...)
  A macro used to fill in the body a function:
    (lambda (arg) _______________)
  So that the function is consistent with the input-output examples provided.

  Return a list with a single element containing one answer.

  We recommend writing `pbe` as a macro, but you can optionally implement `pbe`
  as a function.
|# 


(define (pbe a b )
  (cond [(equal? 0 (length a)) (list 'arg)]
        [(list? (first a) )  ('((list arg)))]
        [(list? (first b) )  ('((list arg)))]
        [(equal? (first a ) (first b) ) (list (first a))]
        
  ))

#;(module+ test
  (test-equal? "pbe identity function"
               (pbe ('x 'x) ('y 'y))
               '(arg))
  (test-equal? "pbe constant function"
               (pbe ('x 'x) ('y 'x))
               '('x))
  (test-equal? "pbe list function"
               (pbe ('x '(x)) ('y '(y)))
               '((list arg))))


