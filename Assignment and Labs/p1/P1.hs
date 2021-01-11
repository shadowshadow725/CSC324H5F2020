{-|
Module: P1 
Description: Project 1: A Spreadsheet Application with DeerLang
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}
-- This lists what this module exports. Don't change this!
module P1
  (
    evalDeer,
    computeSpreadsheet
  )
where

-- You *may not* add imports from Data.Map 
import P1Types(Spreadsheet(..), Definition(..), Column(..),
               Expr(..), Value(..),
               Env, exampleSpreadsheet)
import Prelude hiding (lookup)
import qualified Data.Map (lookup, insert, empty, union)

-------------------------------------------------------------------------------
-- Main Functions: 
-- | These are the functions that we will be directly testing.
-- | Do not change the type signatures of these functions.
-------------------------------------------------------------------------------
evalEnv :: Expr -> [Expr] -> Env -> Env
evalEnv (Lambda local function) lstexpr env = case(length lstexpr) of 
    0 -> env 
    n -> evalEnv (Lambda (tail local) function) (tail lstexpr) (Data.Map.insert (head local) (evalDeer (head lstexpr) env)   env)


evalVal :: Value -> Float
evalVal (VNum f) = f

evalStr :: Value -> String 
evalStr (VStr s) = s

evalBool :: Value -> Bool
evalBool (VBool b) = b

evalClosureVar :: Value -> [String]
evalClosureVar (VClosure s expr env) = s 

evalClosureFunc :: Value -> Value 
evalClosureFunc (VClosure s expr env) = evalDeer expr env 

evalClosureEnv :: Value -> Env 
evalClosureEnv (VClosure s expr env) = env 

evalDeer (Literal v) env = v
evalDeer (Builtin x xs) env = case(x) of 
    "+" ->  if ((length xs) == 2)
      then VNum( evalVal ( evalDeer(head xs) env ) + evalVal ( evalDeer(last xs) env))
      else Error
    "-" ->  if ((length xs) == 2)
      then VNum( evalVal ( evalDeer(head xs) env ) - evalVal ( evalDeer(last xs) env))
      else Error
    "*" ->  if ((length xs) == 2)
      then VNum( evalVal ( evalDeer(head xs) env ) * evalVal ( evalDeer(last xs) env))
      else Error
    "/" ->  if ((length xs) == 2)
      then VNum( evalVal ( evalDeer(head xs) env ) / evalVal ( evalDeer(last xs) env))
      else Error
    "=" ->  if ((length xs) == 2)
      then VBool( evalVal ( evalDeer(head xs) env ) == evalVal ( evalDeer(last xs) env))
      else Error
    ">" ->  if ((length xs) == 2)
      then VBool( evalVal ( evalDeer(head xs) env ) > evalVal ( evalDeer(last xs) env))
      else Error
    ">=" ->  if ((length xs) == 2)
      then VBool( evalVal ( evalDeer(head xs) env ) >= evalVal ( evalDeer(last xs) env))
      else Error
    "++" ->  if ((length xs) == 2)
      then VStr( evalStr ( evalDeer(head xs) env ) ++ evalStr ( evalDeer(last xs) env))
      else Error
    "!" -> if ((length xs) == 1)
      then VBool(not $ evalBool ( evalDeer(head xs) env ))
      else Error
    _ -> Error
evalDeer (Lambda s expr) env =  evalDeer expr env
evalDeer (Apply func args) env = case((length args)) of
    0 -> (evalDeer func env)
    n -> (evalDeer func (evalEnv func args env))

    
computeEnvHelper :: Definition -> Env -> Env
computeEnvHelper (Def key expr) env = (Data.Map.insert  key (evalDeer expr env) env )


computeEnv :: [Definition] -> Env -> Env
computeEnv def env = case(length def) of 
    0 -> env 
    n -> computeEnv (tail def) (computeEnvHelper (head def) env)

computeColumnHelper :: Column -> Env -> [Column] -> [Column]
computeColumnHelper (ValCol key values) env computedColumn = [(ValCol key values)]
computeColumnHelper (ComputedCol key expr) env computedColumn = case(length computedColumn) of 
    0 -> [ValCol key [Error]]
    n -> (findAndComputeColumn (ComputedCol key expr) env computedColumn )


findAndComputeColumn ::  Column -> Env -> [Column] -> [Column]
findAndComputeColumn (ComputedCol key expr) env computedColumn = [] -- not implemented 


computeColumn :: [Column] -> Env -> [Column] -> [Column] 
computeColumn column env computedColumn = case(length column) of 
    0 -> computedColumn
    n -> computeColumn (tail column) env (computedColumn ++ computeColumnHelper (head column) env column)


computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) = (computeColumn columns (computeEnv defs Data.Map.empty) [])



-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

-- Return an environment with the appropriate identifier-to-value bindings.
getEnvironment:: Definition -> Env
getEnvironment def = undefined

-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.

buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs columns env = undefined


-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
