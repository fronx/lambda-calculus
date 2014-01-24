module Untyped where

import Prelude hiding (head)

data Error = Error String
type VarName = String

data Param = Param VarName
           deriving (Show, Eq)

data Term = Var VarName
          | Λ Param Term
          | Term :@ Term
          | Atom Int
          | InvalidTerm String
          deriving (Show, Eq)

replace :: VarName
        -> Term -- new
        -> Term -- term
        -> Term -- result
replace p arg (Atom x) = Atom x
replace p arg (Var x)
  | p == x       = arg
  | otherwise    = Var x
replace p arg (Λ param term)
  | Param p' <- param
  , p == p'
  = Λ param term
  | otherwise = Λ param (replace p arg term)
replace p arg (t1 :@ t2) = (replace p arg t1) :@ (replace p arg t2)
replace p arg (InvalidTerm msg) = InvalidTerm msg

apply :: Term -> Term -> Term
apply (Λ (Param pname) body) term =
  replace pname term body
apply (t1 :@ t2) t3 =
  apply (apply t1 t2) t3
apply a b = InvalidTerm $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

evalSmallStep :: Term -> Term
evalSmallStep (t1 :@ t2) = apply t1 t2
evalSmallStep t = t

eval :: Term -> Term
eval t = case evalSmallStep t of
  (t1' :@ t2') -> eval (t1' :@ t2')
  t -> t

run :: (Int, Term) -> IO ()
run (n, t) = putStrLn $ (show n) ++ ": " ++ (show (eval t))

main = do
  undefined
  --mapM run
  --  [ ( 1, identity)              -- shouldn't change anything
  --  , ( 2, identity :@ (Var "a")) -- Var "a"
  --  , ( 3, constant :@ (Var "a")) -- Var "y"
  --  , ( 4, identity :@ constant)  -- constant
  --  , ( 5, fnfn :@ (Var "a"))     -- the result of this is meaningless
  --  , ( 6, Λ (Param "y") (identity :@ (Var "y")))
  --  , ( 7, fnfn)
  --  , ( 8, identity :@ (Atom 1))
  --  , ( 9, (second :@ (Atom 5)) :@ (Atom 6))
  --  , (10, (first  :@ (Atom 5)) :@ (Atom 6))
  --  , (11, head :@ (cons :@ (Atom 3) :@ (Atom 2)))
  --  ]
