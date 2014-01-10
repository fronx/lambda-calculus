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
          deriving (Show, Eq)

replace :: Term -- old
        -> Term -- new
        -> Term -- term
        -> Term -- result
replace a b (Atom x) = Atom x
replace a b (Var x)
  | a == (Var x) = b
  | otherwise    = Var x
replace a b (Λ param term)
  | a == term = Λ param b
  | otherwise = Λ param (replace a b term)
replace a b (t1 :@ t2) = (replace a b t1) :@ (replace a b t2)

apply :: Term -> Term -> Term
apply (Λ (Param pname) body) term =
  replace (Var pname) term body
apply (t1 :@ t2) t3 =
  apply (apply t1 t2) t3
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

evalSmallStep :: Term -> Term
evalSmallStep (t1 :@ t2) = apply t1 t2
evalSmallStep t = t

eval :: Term -> Term
eval t = case evalSmallStep t of
  (t1' :@ t2') -> evalSmallStep (t1' :@ t2')
  t -> t

run :: (Int, Term) -> IO ()
run (n, t) = putStrLn $ (show n) ++ ": " ++ (show (eval t))

------- LIB -------
identity = Λ (Param "x") (Var "x")
constant = Λ (Param "x") (Var "y")

fnfn = Λ (Param "f")
        (Λ (Param "x")
          ((Var "f") :@ ((Var "f") :@ (Var "x"))))

cons = Λ (Param "a")
        (Λ (Param "b")
          (Λ (Param "f")
            ((Var "f") :@ (Var "a") :@ (Var "b"))))

first  = Λ (Param "a") (Λ (Param "b") (Var "a"))
second = Λ (Param "a") (Λ (Param "b") (Var "b"))

head = Λ (Param "c") (Var "c" :@ first)
tail = Λ (Param "c") (Var "c" :@ second)
------- /LIB -------

main = do
  mapM run
    [ ( 1, identity)              -- shouldn't change anything
    , ( 2, identity :@ (Var "a")) -- Var "a"
    , ( 3, constant :@ (Var "a")) -- Var "y"
    , ( 4, identity :@ constant)  -- constant
    , ( 5, fnfn :@ (Var "a"))     -- the result of this is meaningless
    , ( 6, Λ (Param "y") (identity :@ (Var "y")))
    , ( 7, fnfn)
    , ( 8, identity :@ (Atom 1))
    , ( 9, (second :@ (Atom 5)) :@ (Atom 6))
    , (10, (first  :@ (Atom 5)) :@ (Atom 6))
    , (11, head :@ (cons :@ (Atom 3) :@ (Atom 2)))
    ]
