#lang racket #| CSC324 Fall 2020: Exercise 7 |# 
#|
Please note that you *may* use `eval` on Task 2 of this exercise.
Our starter code has used it in one place below (see `evaluate`),
and you may choose to use it in other places as well, especially if
you use a different approach.
|#

(require "stream.rkt")

(provide s-map s-filter s-stutter continuations)

;-------------------------------------------------------------------------------
; * Task 1: Streams *
;-------------------------------------------------------------------------------

#|
(s-map fn stream) -> stream
  fn: procedure? 
  stream: stream?

  Returns a list containing the values in this stream, each repeated
  n times
|#
(define (s-map-helper fn stream lst)
  (if (equal? '() (cdr stream)) (reverse (cons (fn ((car stream))) lst)) (s-map-helper fn ((cdr stream)) (cons (fn ((car stream))) lst)))
  )

(define (s-map fn stream)
  (if (s-null stream)
      '()
      (s-map-helper fn stream '())
      ))




#|
(s-filter fn stream) -> stream
  fn: procedure? 
  stream: stream?

  Returns a list containing the values in this stream, each repeated
  n times
|#
(define (s-filter-helper fn stream lst)

(if (equal? '() (cdr stream))
    (if (fn ((car stream))) (reverse (cons ((car stream)) lst)) lst )
    (s-filter-helper fn ((cdr stream))
    (if (fn ((car stream)))
        (cons ((car stream)) lst)
        lst
        )
    )
   )
)

 

(define (s-filter fn stream)
  (if (s-null stream)
      '()
      (s-filter-helper fn stream '()))

  )

#|
(s-stutter stream n) -> stream?
  stream: stream?
  n: integer

  Returns a list containing the values in this stream, each repeated
  n times
|#
(define (stutter n word lst)

(if (equal? n 0)
    lst
    (stutter (- n 1) word (cons word lst))

 ))

(define (s-stutter-helper stream n lst)

(if (equal? '() (cdr stream))
    (reverse (append (stutter n ((car stream)) '()) lst))
    (append (stutter n ((car stream)) '()) (s-stutter-helper ((cdr stream)) n lst) )
    )
 
)

(define (s-stutter stream n)
  (if (s-null stream)
      '()
      (s-stutter-helper stream n '())
      )

  )


;-------------------------------------------------------------------------------
; * Task 2: Displaying Continuations *
;-------------------------------------------------------------------------------

#|
(continuations expr) -> (listof (listof datum?)) 
  expr: datum?
    A Racket datum consisting of only nested + expressions and numeric literals.

  Returns a list of *continuation pairs* for each subexpression of `expr`,
  including `expr` itself.

  Each continuation pair is a list with two elements:
    - The first element is a datum that is a subexpression of `expr`.
    - The second element is a datum representing the first element's *continuation*
      with respect to `expr`, in the form described on the exercise handout.

  The continuation pairs should be returned in the order the subexpressions
  would be evaluated in Racket (i.e., obeying left-to-right eager evaluation for function calls).
  See the provided sample tests at the bottom of this file for some examples.
|#
(define (continuations expr)
  (if (list? expr)
      ; Complete the case where the datum is a list. We've provided a
      ; call to `accumulate-continuations` that you may or may not use as a
      ; starting point.
      (accumulate-continuations expr)

      ; The "base case" is when the outermost expression is atomic.
      ; In this case what should be returned? (See sample tests.)
      (list (list expr '_))
      ))

#|
(accumulate-continuations expr)
  expr: A datum

  A wrapper for the main suggested helper. This function basically does a `foldl`
  to accumulate the continuation pairs for each subexpression in `expr`.
  There are two subtleties, however, that make this structure a little unusual:

  1. Computing the correct continuation of a subexpression requires knowing
     what the "full" expression is, since that's the whole point of a continuation.
  2. Because we want to handle nested subexpressions and evaluation order
     (see last sample test case below), we can't treat `expr` as static;
     we need to make updates as we traverse the subexpressions.

  Because of this, our foldl uses an accumulator that has *two values*:
    - a list of continuation pairs (this is pretty standard)
    - the current state of the expression

  Rather than fold over the datum directly, we call `foldl` on a list of the
  *indexes* into `expr`.
|#
(define (accumulate-continuations expr)
  (append
   (first (foldl continuations-for-position
         ; Accumulator starts with no continuation pairs and the original expression.
         (list (list) expr)

         ; Fold over indexes into `expr`.
         (range (length expr)))
         )
   (list (list expr '_))
   )
)


#|
(continuations-for-position i acc)
  i: A non-negative integer.
  acc: A list of two values: a list of continuation pairs and a datum.

  Precondition: i is a valid (in-bounds) index into the datum in acc.

  Returns a list of two updated values:
    - An updated list of continuation pairs, with the new pairs being
      the ones from the i-th subexpression in `(second acc)`.
    - An updated datum that represents the new state of the datum after
      its i-th subexpression is evaluated.

  Note: if you're having trouble wrapping your head around the "updated datum"
  part, you can simply use (second acc)` as the second element of the list
  that you return; i.e., keep the datum the same for all iterations of the
  above `foldl` call. You'll fail the last sample test, but should be able 
  to pass all the others.
|#
(define
  (some-helper expr fn lst) (
    if (equal? 0 (length expr))
       (reverse lst)
       (some-helper (rest expr) fn (cons (list (first (first expr)) (fn (second (first expr)))) lst )))
  )

 
(define/match (just-eval expr)
  [((list '+ x y)) ( + (just-eval x) (just-eval y))]
  [((list '- x y)) ( - (just-eval x) (just-eval y))]
  [((list '* x y)) ( * (just-eval x) (just-eval y))]
  [((list '/ x y)) ( / (just-eval x) (just-eval y))]
  [(x) x]
  )

(define (continuations-for-position i acc)
  (let* (; The current continuation pairs and expression datum
         [current-pairs (first acc)]
         [current-expr (second acc)]

         ; The i-th subexpression
         [subexpr-i (list-ref current-expr i)]

         ; A potentially-useful helper. But it's up to you to use it!
         [substitute-i (make-substituter current-expr i)]

         ; This recursive call is a bit subtle. As noted above, just calling
         ; `continuations` on the subexpressions does not actually yield
         ; the correct continuations, because you need to know how `subexpr-i`
         ; sits inside `current-expr`. So `pairs-i` is a good first step,
         ; but it's up to you to fix it before appending it to `current-pairs`.
         [pairs-i (if (list? subexpr-i) (
                                         accumulate-continuations subexpr-i

                                                                  ) '())])
    
    ; Return a new list containing the updated accumulator (remember that
    ; you're returning two values here).
    (if (list? subexpr-i)
        (list (append current-pairs  (some-helper pairs-i substitute-i '() )) (substitute-i (just-eval subexpr-i)))
        (list (append current-pairs (list (list subexpr-i (substitute-i '_)))) current-expr)
        )
    ;(list (append current-pairs (list (list subexpr-i (substitute-i '_)))) current-expr)

    ))


;-------------------------------------------------------------------------------
; Helpers (provided for you, but you can change or add to them as necessary).
;-------------------------------------------------------------------------------
#|
(make-substituter expr i)
  expr: A datum
  i: An integer

  Precondition: i is a valid index into expr. (And consequently, expr is a
  list, not an atomic datum.)

  Returns a function that takes a datum and substitutes it into `expr`
  in position `i` (replacing whatever was in position `i` before.
|#
(define (make-substituter expr i)
  ; Tip: look up the built-in `curry` and review currying!
  (curry list-set expr i))


#|
(evaluate expr)
  expr: A datum

  Evaluates the given datum by cheating (using Racket's `eval` function),
  to avoid including our own interpreter. Also preserves '+ as a symbol
  rather than evaluating (makes the datum output a little simpler).
|#
(define (evaluate expr)
  (cond
    [(number? expr) expr]
    [(symbol? expr) expr]
    [else (eval expr (make-base-namespace))]))


;-------------------------------------------------------------------------------
; Sample tests
;-------------------------------------------------------------------------------
(module+ test
  (require rackunit)

  (test-equal?
   "Continuation for a numeric literal."
   (continuations '3)  ; Note: remember that '3 == 3 (true for all literals).
   (list (list '3 '_))  ; Only one expression, so only one pair.
   )

  (test-equal?
   "Continuation for the standalone + function."
   ; Note that types don't matter; a top-level '+ has the same continuation
   ; as a top-level '3.
   (continuations '+)
   (list (list '+ '_)))

  (test-equal?
   "Continuations for a single + operation."
   (continuations '(+ 3 4))
   (list (list '+        '(_ 3 4))
         (list '3        '(+ _ 4))
         (list '4        '(+ 3 _))
         (list '(+ 3 4)  '_      )  ; Don't forget about the entire expression!
         ))

  (test-equal?
   "Continuations for a nested expression (part 1)."
   ; This test will pass even if you're using the same `current-expr`
   ; all the way through the helper.
   (take (continuations '(+ (+ 2 3) (+ 8 9))) 5)
   (list (list '+                    '(_ (+ 2 3) (+ 8 9)))
         (list '+                    '(+ (_ 2 3) (+ 8 9)))
         (list '2                    '(+ (+ _ 3) (+ 8 9)))
         (list '3                    '(+ (+ 2 _) (+ 8 9)))
         (list '(+ 2 3)              '(+ _       (+ 8 9)))))

  (test-equal?
   "Continuations for a nested expression (part 2)."
   ; This test requires you to successfully evaluate (+ 2 3) into 5
   ; when computing continuations of the (+ 8 9) subexpression.
   (drop (continuations '(+ (+ 2 3) (+ 8 9))) 5)
   (list (list '+                    '(+ 5       (_ 8 9)))
         (list '8                    '(+ 5       (+ _ 9)))
         (list '9                    '(+ 5       (+ 8 _)))
         (list '(+ 8 9)              '(+ 5       _      ))
         (list '(+ (+ 2 3) (+ 8 9))  '_                  ))))
