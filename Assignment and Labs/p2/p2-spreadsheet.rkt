#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt") ; (require "p2-soln.rkt")

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#

(define (lookup key alst)
  (if (and (< 0 (length alst)) (equal? (car (first alst)) key))
      (if (list? (first (cdr (first alst))))
          (first (cdr (first alst)))
          (cdr (first alst)))
      (if (<= (length alst) 1)
          'error
          (lookup key (rest alst))
          )
      ))

(define/match (types-of-func expr env)
  [((list '+ x y) env ) '(num num num)]
  [((list '- x y) env ) '(num num num)]
  [((list '* x y) env ) '(num num num)]
  [((list '/ x y) env ) '(num num num)]
  [((list '> x y) env ) '(num num bool)]
  [((list '= x y) env ) '(num num bool)]
  [((list '>= x y) env ) '(num num bool)]
  [((list '++ x y) env ) '(str str str)]
  [((list '! x ) env ) '(bool bool)]
  [((list 'num->str x ) env ) '(num str)]
  [((list 'len x ) env ) '(str num)]
  [(x env)  '(error)]
  )


(define sh '(spreadsheet
(def (voting-age 18)
(canvote (lambda (x) (>= x voting-age))))
(columns
(name str (values "adam" "betty" "clare" "eric" "sam"))
(age num (values 12 15 18 49 17))
(voter bool (computed (canvote age)))
(voter2 bool (computed (>= age name))))))

(define (make-env def)
  (list (cons (first (list-ref (list-ref sh 1) 1)) (list-ref (list-ref (list-ref sh 1) 1) 1))))

(define (make-func def)
  (list (cons (list-ref (list-ref (list-ref sh 1) 2) 0) (list (list-ref (list-ref (list-ref (list-ref sh 1) 2) 1) 2 )))))

(define (check-column type column env)
  (if (eq? column '())
      #t
      (if (eq? (typeof (first column) env) type)
          (check-column type (rest column) env)
          #f)))
(define (find-type-of-col cols name)
  (if (eq? cols '())
      'error
      (if (eq? name (first (first cols)))
          (list-ref (first cols) 1)
          (find-type-of-col (rest cols) name))))

(define (check-list-of-types type-lst args env)
  (if (and (eq? 0 (length args)) (eq? 0 (length type-lst)))
      (if (eq? (first type-lst) (typeof (first args) env) )
          (check-list-of-types (rest type-lst) (rest args) env) #f) #t))



(define (check-column-func type column env all)
  (let* (
         [t (types-of-func (lookup (first (first column)) env) env)]
         [args (rest column)]
         )
    (if (eq? (last t) type)
        (check-list-of-types t args env)
        #f)))

(define/match (column-type cols env all)
  [((list x type (cons 'values lst)) env all) (check-column type lst env) ]
  [((list x type (cons 'computed lst)) env all)  (check-column-func type lst env all)]
  ;[((list x type (cons 'computed lst)) env all)  #f]
  )

(define (check-all cols ans env all)
  (if (eq? cols '())
      (reverse ans)
      (check-all (rest cols) (cons (column-type (first cols) env all) ans ) env all)))

(define (col-env cols env)
  (if (eq? (length cols) 0)
      env
      (col-env (rest cols) (cons (cons (first (first cols)) (second (first cols))) env))))

(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (list-ref spreadsheet 1)]
         [cols (list-ref spreadsheet 2)]
         [envbind (append (make-func spreadsheet) (make-env spreadsheet))])
    (check-all (rest cols) '() (col-env (rest cols) envbind) spreadsheet)))
;(type-check-spreadsheet sh)
;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (void) ; Don't know what to do 
    ]))


