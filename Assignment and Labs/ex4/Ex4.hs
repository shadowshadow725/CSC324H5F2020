{-|
Module: Ex4
Description: Exercise 4

This part of the exercise is similar in spirit to Exercise 2, except using
the Haskell programming language instead.

The one /new/ idea here is the use of the QuickCheck library to use a different
kind of approach to testing called /property-based testing/. We illustrate a
few simple property-based tests throughout this file.
-}
-- This lists what this module exports. Don't change this!
module Ex4
  (
    calculate,
    evalCalc,
    isPositiveNumber, 
    numLambdas
  )
where

-- You *may* add imports from Data.List, but no other imports.
import Ex4Types (Value(..), Expr(..))
import Prelude hiding (lookup)
import Data.List (sort)
import Data.Map (lookup, empty, Map, insert)
import Test.QuickCheck (Property, quickCheck, (==>))

-------------------------------------------------------------------------------
-- * Task 2: Calculator in Haskell
-------------------------------------------------------------------------------


-- | Warmup task 1: write a function that determines whether
-- a value is a positive number. Use pattern matching!
isPositiveNumber :: Value -> Bool
isPositiveNumber (Number v) = (v > 0)
isPositiveNumber (Closure k expr env) = (isPositiveNumber (evalCalc expr env))
       

isPositiveNumber (_) = False

-- | Warmup task 2: write a function that counts the number of
-- function expressions (Lambdas) in an expression. The Lambdas
-- can appear anywhere in the expression. For example, an expression
-- like (lambda (x) (lambda (y) (+ x y))) contains two lambdas.

numLambdas :: Expr -> Float
numLambdas (Apply fnexpr argexpr) = (numLambdas fnexpr)
numLambdas (Lambda param body) = (numLambdas body)+1
numLambdas (_) = 0

prop_numLambdasOne :: Bool
prop_numLambdasOne = 
    let expr = (Apply (Lambda "x" (Add (Id "x") (Literal $ Number 1)))
                      (Literal $ Number 3))
    in (numLambdas expr) == 1


-- | calculate: take an Expr, and evaluate it to return a number
-- Here you'll need to use pattern-matching on the different forms of an Expr 
-- (@Number@ or @Add@ or @Sub@ ...), because we don't have an explicit "number?"
-- function for this datatype.
calculate :: Expr -> Value
calculate expr = evalCalc expr empty

applyfn :: Expr -> Expr -> (Map String Value) -> (Float -> Float -> Float) -> Value
applyfn e1 e2 env op =
    case ((evalCalc e1 env), (evalCalc e2 env)) of
        ((Number a), (Number b)) -> Number (op a b)
        _                        -> Error

evalCalc :: Expr ->  (Map String Value) -> Value
evalCalc (Literal v) env = v
evalCalc (Id id) env = 
    case (lookup id env) of
        Just value -> value
        Nothing    -> Error
evalCalc (Add e1 e2) env = applyfn e1 e2 env (+)
evalCalc (Sub e1 e2) env = applyfn e1 e2 env (-)
evalCalc (Mul e1 e2) env = applyfn e1 e2 env (*)
evalCalc (Div e1 e2) env = applyfn e1 e2 env (/)
evalCalc (Let [] body) env = evalCalc body env
evalCalc (Let ((id, expr):bindings) body) env =
    -- This is a different approach than what you used in Ex3,
    -- and express let expressions in terms of function expressions
    -- and applications
    let lambdaexpr = Lambda id (Let bindings body)
        argexpr = (Apply lambdaexpr expr)
    in evalCalc argexpr env
-- Complete these two cases
evalCalc (Lambda param body) env = (evalCalc body env)
evalCalc (Apply (Lambda id expr) argexpr) env = ((evalCalc (Lambda id expr)) (Data.Map.insert id (calculate argexpr) env))


---------------------------------------------------------------------------------
---- | test calculate addition
prop_calculateAdd :: Bool
prop_calculateAdd =
    let x = (calculate $ Add (Literal $ Number 3) (Literal $ Number 4))
    in x == (Number 7)

---- | test calculate function
prop_calculateFn :: Bool
prop_calculateFn = 
    let expr = (Apply (Lambda "x" (Add (Id "x") (Literal $ Number 1)))
                      (Literal $ Number 3))
        x = calculate expr
    in x == (Number 4)

-- | test calculate let expressions
prop_calculateLet :: Bool
prop_calculateLet = 
    let expr = (Let [("a", Literal $ Number 3),
                     ("b", Literal $ Number 2)]
                 (Add (Id "a") (Id "b")))
        x = calculate expr
    in x == (Number 5)


-- | todo: write your own tests!

-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_numLambdasOne
  quickCheck prop_calculateAdd
  quickCheck prop_calculateFn
  quickCheck prop_calculateLet

