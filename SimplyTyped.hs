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
replace a b (t1 :@ t2) = (replace a b t1) :@ (replace a b t2)
replace a b t = t

apply :: Term -> Term -> Term
apply (Λ (Param pname ptype) body) term =
  replace (Var pname) term body
apply (t1 :@ t2) t3 =
  apply (apply t1 t2) t3
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

eval :: Term -> Term
eval (t1 :@ t2) =
  case apply t1 t2 of
    (t1' :@ t2') -> eval (t1' :@ t2')
    t -> t
eval t = t

run :: Term -> (Context, Term)
run term = (context, term')
  where
    context = doType emptyContext term
    term'   = case context of
      TypeErrorContext _ -> term
      _ -> eval term

runWithContext :: Show lineId => (lineId, Term) -> IO ()
runWithContext (lineId, term) =
  putStr $
    "\n\n-- " ++ (show lineId) ++ ": " ++ (show term) ++ "\n\n" ++
    (show term') ++ "\n" ++
    (show context)
  where (context, term') = run term

------- LIB -------
int = Type TInt

intIdentity = Λ (Param "x" int) (Var "x")
intConstant = Λ (Param "x" int) (Var "y")

intCons = Λ (Param "a" int)
           (Λ (Param "b" int)
             (Λ (Param "f" (int :-> int :-> int))
               ((Var "f") :@ (Var "a") :@ (Var "b"))))

intFirst  = Λ (Param "a" int) (Λ (Param "b" int) (Var "a"))
intSecond = Λ (Param "a" int) (Λ (Param "b" int) (Var "b"))

intHead = Λ (Param "c" ((int :-> int :-> int) :-> int)) (Var "c" :@ intFirst)
intTail = Λ (Param "c" ((int :-> int :-> int) :-> int)) (Var "c" :@ intSecond)
------- /LIB -------

main = do
  let fnfn = Λ (Param "f" (int :-> int))
               (Λ (Param "x" int)
                 ((Var "f") :@ ((Var "f") :@ (Var "x"))))

  mapM runWithContext
    [ ( 1, Var "x") -- error: x hasn't been declared
    , ( 2, intIdentity)
    , ( 3, Λ (Param "y" int) (intIdentity :@ (Var "y")))
    , ( 4, intConstant)
    , ( 5, fnfn)
    , ( 6, fnfn :@ (Var "a")) -- error
    , ( 7, fnfn :@ (Λ (Param "a" (Type TBool)) (Var "a"))) -- error
    , ( 8, intIdentity :@ (Var "a")) -- error
    , ( 9, fnfn :@ (Λ (Param "a" int) (Var "a"))) -- error
    , (10, intHead :@ (intCons :@ (VInt 3) :@ (VInt 2)))
    ]
