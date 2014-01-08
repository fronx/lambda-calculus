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

apply :: Term -> Term -> Term
apply (Λ (Param pname ptype) body) term =
  replace (Var pname) term body
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

eval :: Term -> Term
eval (Apply t1 t2) = apply t1 t2
--eval (Λ param body) = undefined -- TODO
eval t = t

run :: Term -> (Context, Term)
run term =
  (context, term')
  where
    context = doType emptyContext term
    term'   = case context of
      TypeErrorContext _ -> term
      _ -> eval term

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

  mapM runWithContext
    [ (1, Var "x") -- error: x hasn't been declared
    , (2, identity)
    , (3, Λ (Param "y" int) (Apply identity (Var "y")))
    , (4, other)
    , (5, fnfn)
    , (6, Apply fnfn (Var "a")) -- error
    , (7, Apply fnfn (Λ (Param "a" (Type "foo")) (Var "a"))) -- error
    , (8, Apply identity (Var "a")) -- error
    , (9, Apply fnfn (Λ (Param "a" (Type "int")) (Var "a"))) -- error
    ]
  where
    runWithContext (lineNum, term) =
      putStr $
        "\n\n-- " ++ (show lineNum) ++ ": " ++ (show term) ++ "\n\n" ++
        (show term') ++ "\n" ++
        (show context)
      where (context, term') = run term
