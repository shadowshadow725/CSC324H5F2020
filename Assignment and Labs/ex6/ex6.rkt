#lang racket #| CSC324 Fall 2020: Exercise 6 |# 

;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide my-or* my-and* lazy-curry Walrus my-class my-class-getter)

(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------
; * Task 1: Macro Practice *
;-------------------------------------------------------------------------------

#|
(my-or* p ...)
  p ... : booleans

  A generalization of my-or to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `or` (for boolean arguments).

  This macro should be *recursive* (i.e., use itself in a template).
  We've illustrated the basic pattern-matching forms below.
|#
(define-syntax my-or*
  (syntax-rules ()
    [(my-or*) #f]
    [(my-or* <x> <p> ...) (if <x> #t (my-or* <p> ...))]
    ))

#|
(my-and* p ...)
  p ... : booleans

  A generalization of my-and to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `and` (for boolean arguments).
|#
(define-syntax my-and*
  (syntax-rules ()
    [(my-and*) #t]
    [(my-and* <x> <p> ...) (if <x> (my-and* <p> ...) #f)]
    ))

#|
(lazy-curry fn args ...)
  fn args ... : procedure

  A generalization of my-and to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `and` (for boolean arguments).
|#
(define-syntax lazy-curry 
  (syntax-rules ()
    [(lazy-curry <f>) <f>]
    [(lazy-curry <f> <a> ...)  (lambda x ((lambda xs (apply <f> (first xs))) (append '(<a> ...) x) )) ] 
    ))

(module+ test
   (define (f x y z) (+ (* x y) z))
   (define g1 (lazy-curry f 3))
   (define g2 (lazy-curry f 3 4))
   (test-true "g1 and g2 are functions" (and (procedure? g2) (procedure? g2)))
   (test-equal? "g1" (g1 4 5) 17)
   (test-equal? "g2" (g2 5) 17))

;-------------------------------------------------------------------------------
; * Task 2: Working with objects *
;-------------------------------------------------------------------------------
#;(define-syntax my-class
  (syntax-rules (method)
    [(my-class <class-name>
       (<attr> ...)
       (method (<method-name> <param> ...) <body>)
       ...)
     (define (<class-name> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (quote <attr>)) <attr>]
               ...
               [(equal? msg (quote <method-name>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]))

(define-syntax my-class
  (syntax-rules (method)
    [(my-class <class-name> (<attr> ...)
       (method (<method-name> <param> ...) <body>) ...
       )
     (define (<class-name> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (quote <attr>)) <attr>]
               ...
               [(equal? msg (quote <method-name>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]))

 ;bucket-empty? same-name? equal? catch have-birthday
(my-class Walrus (name age bucket)
          (method (bucket-empty? ) (equal? bucket '()) )
          (method (same-name? other ) (equal? name (other 'name)) )
          (method (equal? other ) (and (equal? name (other 'name)) (equal? age (other 'age))) )
          (method (catch item) (Walrus name age (cons item bucket)) )
          (method (have-birthday) (Walrus name (+ age 1) (cons 'cake bucket)) )
          )
          
           

(module+ test
  (define alice (Walrus "alice" 4 '()))
  (define bob (Walrus "alice" 4 '()))

  (test-true "alice and bob are both functions"
              (and (procedure? alice) (procedure? bob)))
  (test-true "alice and bob have different names"
              ((alice 'same-name?) bob))
  ; TODO: add more tests here!
)

;-------------------------------------------------------------------------------
; * Task 3: Accessor function in `my-class` *
;-------------------------------------------------------------------------------

#|
(my-class-getter <Class> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from above.

  In addition to defining the constructor, my-class-getter defines
  *one new accessor function per attribute of the class*, using the name
  of the attribute as the name of the function.

  Implementation notes:
    - Our starter code has wrapped the `define` from lecture inside a `begin`.
      This is required so that you can add more `define`s after the one for the
      constructor.
|#
(define-syntax my-class-getter
  (syntax-rules (method)
    [(my-class-getter <class-name>
       (<attr> ...)
       (method (<method-name> <param> ...) <body>)
       ...)
     (begin
       (define (<class-name> <attr> ...)
         (lambda (msg)
           (cond [(equal? msg (quote <attr>)) <attr>]
                 ...
                 [(equal? msg (quote <method-name>))
                  (lambda (<param> ...) <body>)]
                 ...
             [else "Unrecognized message!"])))
       (define <attr> (lambda (classname) (classname (quote <attr>))) ) ...

                          
         
       ; It is possible to do this assignment by only adding new
       ; expression(s) here!
     )]))
       
(module+ test
  ; We use `local` to create a local scope for our definitions.
  ; Run these tests when you're ready!
  (local
      [(my-class-getter Point (x y))]
      (test-true "x and y are functions" (and (procedure? x) (procedure? y)))
      (test-equal? "x and y are accessors"
                   (let ([p (Point 2 3)])
                     (list (x p) (y p)))
                   (list 2 3)))
)



