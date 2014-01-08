module SimplyTyped where

import Data.List
import Types
import Typing

replace :: Term -- old
        -> Term -- new
        -> Term -- term
        -> Term -- result
replace a b (Var x)
  | a == (Var x) = b
  | otherwise    = Var x
replace a b (Λ param term)
  | a == term = Λ param b
  | otherwise = Λ param (replace a b term)
replace a b (Apply t1 t2) = Apply (replace a b t1) (replace a b t2)

failApply :: Term -> Term -> String
failApply a b = "Can't apply " ++ (show a) ++ " to " ++ (show b)

apply :: Term -> Term -> Either String Term
apply (Λ (Param pname ptype) body) term =
  Right $ replace (Var pname) term body
apply a b =
  Left $ failApply a b

eval :: Term -> Either String Term
eval (Apply t1 t2) = apply t1 t2
eval t = Right t

evalAndPrint :: Term -> IO ()
evalAndPrint term =
  case eval term of
    Left msg    -> putStrLn msg
    Right term' -> print term'

printEither :: Show b => Either String b -> IO ()
printEither x =
  case x of
    Left msg -> putStrLn msg
    Right x' -> print x'

---

main = do
  let int = Type "int"
  let x = Param "x" int
  let f = Param "f" (int :-> int)

  let identity = Λ x (Var "x")
  let other    = Λ x (Var "y")

  let fnfn = Λ f
               (Λ x (Apply (Var "f")
                           (Apply (Var "f")
                                  (Var "x"))))
  print fnfn

  print "---"
  evalAndPrint $
    identity -- shouldn't change anything
  evalAndPrint $
    Apply identity (Var "a") -- Var "a"
  evalAndPrint $
    Apply other (Var "a") -- other
  evalAndPrint $
    Apply identity other -- other
  evalAndPrint $
    Apply fnfn (Var "a") -- the result of this is meaningless
  evalAndPrint $
    Apply (Var "x") (Var "a") -- error message

  print $ doType emptyContext (Var "x") -- error: x hasn't been declared
  print $ doType emptyContext identity
  print $ doType emptyContext other
  print $ doType emptyContext fnfn
  print $ doType emptyContext (Apply fnfn (Var "a")) -- error
  print $ doType emptyContext (Apply fnfn (Λ (Param "a" (Type "foo")) (Var "a"))) -- error
  print $ doType emptyContext (Apply fnfn (Λ (Param "a" (Type "int")) (Var "a"))) -- error
