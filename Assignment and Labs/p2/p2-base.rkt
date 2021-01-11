#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#

(define/match (typeofhelper expr env)
  [((list '+ x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'num 'error)]
  [((list '- x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'num 'error)]
  [((list '* x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'num 'error)]
  [((list '/ x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'num 'error)]
  [((list '> x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'bool 'error)]
  [((list '= x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'bool 'error)]
  [((list '>= x y) env ) (if (and (eq? 'num (typeof x env))  (eq? 'num (typeof 'y env))) 'bool 'error)]
  [((list '++ x y) env ) (if (and (eq? 'str (typeof x env))  (eq? 'str (typeof 'y env))) 'str 'error)]
  [((list '! x ) env ) (if (eq? 'bool (typeof 'x env)) 'bool 'error)]
  [((list 'num->str x ) env ) (if (eq? 'num (typeof 'x env)) 'str 'error)]
  [((list 'len x ) env ) (if (eq? 'str (typeof 'x env)) 'num 'error)]
  [(x env)  'error]
  )

(define (typeof expr typeenv)
  (cond
    ; Constants
   
    [(number? expr) 'num]
    [(string? expr) 'str]
    [(boolean? expr) 'bool]
    ; Identifiers
    ; TODO
    [(symbol? expr) (lookup expr typeenv)]
    ; Builtins
    ; TODO
    [(list? expr) (typeofhelper expr typeenv)]

    ; Function Calls
    ; TODO
    [else 'error]
  ))

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup key alst)
  (if (and (> 0 (length alst)) (equal? (car (first alst)) key))
      (cdr (first alst))
      (if (<= (length alst) 1)
          'error
          (lookup key (rest alst))
          )
      ))

; Add your helper functions here

;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
    ; constants: numbero, stringo, and boolo are miniKanren builtin relations
    ((numbero expr)
     (== type 'num))
    ((stringo expr)
     (== type 'str))
    ((conde
     ((== expr #f))
     ((== expr #t)))
     (== type 'bool))
   
   
    ; identifier: symbolo is a miniKanren builtin relation
    ; TODO
    ((symbolo expr)
     (lookupo expr env type)
     )
    ; builtins
    ; TODO
    
    ((fresh (arg1 arg2 arg3 )
                   (== expr (list arg1 arg2 arg3))
                   (conde (
                   (conde (
                           (conde
                            ((== arg1 '+))
                            ((== arg1 '-))
                            ((== arg1 '/))
                            ((== arg1 '*)))
                           (conde
                            ((typeo arg2 env 'num)
                             (typeo arg3 env 'num)
                             (== type 'num))))))
                   ((conde((conde
                           ((== arg1 '>))
                           ((== arg1 '=))
                           ((== arg1 '>=)))
                          (conde
                           ((typeo arg2 env 'num)
                            (typeo arg3 env 'num)
                            (== type 'bool))
                           )))))))
     ((fresh (arg1 arg2)
                   (== expr (list arg1 arg2))
                   (conde (
                           (conde ((== arg1 '!)
                                  (typeo arg2 env 'bool)
                                  (== type 'bool)))
                           (conde ((== arg1 'num->str)
                                  (typeo arg2 env 'num)
                                  (== type 'str)))
                           (conde ((== arg1 'len)
                                  (typeo arg2 env 'str)
                                  (== type 'num)))
                           ))))
     ((fresh (arg1 argrest lam localargs func newenv)
             (== expr (cons arg1 argrest))
             (== arg1 (list lam localargs func))
             (makeenvo localargs argrest env newenv)
             (typeo func newenv type)))
     
     

    ; function calls
    ; TODO

    ; function definitions
    ; TODO
    ))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#

;(run 1 (out) (typeo 'a '((a . num) (b. str)) out))
; (run 1 (out) (lookupo 'a '((a . num) (b. str)) out))
(define (lookupo key alst value)
  (conde((fresh (elm1 elmrest fkey fvalue)
                (== alst (cons elm1 elmrest))
                (== elm1 (cons fkey fvalue))
                (conde ((== key fkey)
                        (== value fvalue))
                       ((lookupo key elmrest value)))
                
                
                )
         )))


(define (makeenvo key lst env result)
  (conde ((fresh (fkey rkey flst rlst)
                 (== key (cons fkey rkey))
                 (== lst (cons flst rlst))
                 (conde
                  ((makeenvo rkey rlst (cons (cons fkey 'num) env) result)
                   (numbero flst))
                  ((makeenvo rkey rlst (cons (cons fkey 'str) env) result)
                   (stringo flst))
                  ((makeenvo rkey rlst (cons (cons fkey 'bool) env) result)
                   (conde ((== flst #t)) ((== flst #f))))
                  ((== '() rlst)
                   (== '() rkey)
                   (conde ((== result (cons (cons fkey 'num) env)) (numbero flst))
                          ((== result (cons (cons fkey 'str) env)) (stringo flst))
                          ((== result (cons (cons fkey 'bool) env)) (conde ((== flst #t)) ((== flst #f))))
                          )
                   )
                  )))))
(define (firsto lst result)
  (conde ((fresh (fo)
          ((== lst (cons fo '()))
          (== fo result))))))


; Add your helper functions here

