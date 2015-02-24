module LcPre where

import Prelude hiding (lookup)

-- A simple interpreter for lambda calculus

type Name = String

-- A term is either a variable, a constant, the addition of two term,
-- a lambda expression or the application of a lambda expression to another term
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

-- A value is the resut of the interpretation of a term, and is either
-- wrong (when the interpreter results to an error such as an unbound
-- variable, adding non numbers or applying a non function), a number
-- or a function.
data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

-- The environment is a list of pairs consisting with the name of a
-- bound variable and the value 
type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun _) = "<function>"

-- Interprets a term to a value ina given environment
interp :: Term -> Environment -> Value
-- A variable is interpreted to the bounded value
interp (Var x) e = lookup x e
-- A constant is interpreted to a number 
interp (Con i) _ = Num i
-- adding two terms is interpreted as adding the interpretation of
-- each term
interp (Add u v) e = add (interp u e) (interp v e)
-- a lambda expression is interpreted as a function; the body of the
-- function is the interpretation of the second term given an
-- enriched environment with the argument of the lambda expression bounded
-- to the argument of the function
interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
-- an appliction is interpreted applying the function resulting from
-- the interpretation of the first term to the value resulting from
-- the interpretation of the second term
interp (App t u) e = apply (interp t e) (interp u e)

lookup :: Name -> Environment -> Value
lookup _ [] = Wrong
lookup x ((y,b):e) = if x == y then b else lookup x e

add :: Value -> Value -> Value
add (Num i) (Num j) = Num (i + j)
add _ _ = Wrong

apply :: Value -> Value -> Value
apply (Fun k) a = k a
apply _ _ = Wrong

test :: Term -> String
test t = showval (interp t [])

term0 :: Term
term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))

test0 :: Bool
test0 = 
    -- a bound variable: in the environment the variable x is bound to
    -- number 1.
    showval (interp (Var "x") [("x", Num 1)]) == "1"
    -- lookup "x" [("x", Num 1)]
    -- Num 1
    && showval (Num 1) == "1"

    -- an anbound variable: in the environment the variable x is bound
    -- to no value
    && showval (interp (Var "x") []) == "<wrong>"
    -- lookup "x" [] 
    -- Wrong
    && showval Wrong == "<wrong>"

    -- an number (constant): a constant is interpreted as a number
    -- regardless of the environment
    && showval (interp (Con 1) []) == "1"
    -- Num 1

    -- an addition: addition of two constant is interpreted as
    -- addition of two numbers: 
    && showval (interp (Add (Con 1) (Con 2)) []) == "3" 
    -- add (Num 1) (Num 2)
    -- Num 3

    && showval (interp (Add (Con 1) (Var "x")) [("x", Num 2)]) == "3"
    -- add (Num 1) (lookup "x" [("x", Num 2)])
    -- add (Num 1) (Num 2)
    -- Num 3

    -- addition of non-number: 
    && showval (interp (Add (Con 1) (Lam "x" (Con 2))) []) == "<wrong>"
    -- add (Num 1) (Fun (\a -> Num 2))
    -- Wrong

    -- a lambda expression: 
    && showval (interp (Lam "x" (Add (Var "x") (Var "x"))) []) == "<function>"
    -- Fun (\a -> add (lookup "x" [("x",a)]) (lookup "x" [("x",a)]))
    -- Fun (\a -> add a a)
    && showval (Fun (\a -> add a a)) == "<function>"
       
    -- an application
    && showval (interp (App (Lam "x" (Var "x")) (Con 1)) []) == "1"
    -- apply (Fun (\a -> lookup "x" [("x",a)])) (Num 1)
    -- (\a -> a) (Num 1)
    -- Num 1

    && test term0 == "42"

