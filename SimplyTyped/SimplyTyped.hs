module SimplyTyped
  ( replace
  , apply
  , evalSmallStep
  , eval
  , run
  , runWithContext
  , module Types
  , module Typing
  )
where

import Types
import Typing

replace :: VarName
        -> Term -- new
        -> Term -- term
        -> Term -- result
replace p arg (Var x)
  | p == x    = arg
  | otherwise = Var x
replace p arg (Λ param term)
  | Param p' _ <- param
  , p == p'
  = Λ param term
  | otherwise = Λ param (replace p arg term)
replace p arg (t1 :@ t2) = (replace p arg t1) :@ (replace p arg t2)
replace p arg t = t

apply :: Term -> Term -> Term
apply (Λ (Param pname _) body) arg =
  replace pname arg body
apply (t1 :@ t2) t3 =
  apply (apply t1 t2) t3
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

evalSmallStep :: Term -> Term
evalSmallStep (t1 :@ t2) = apply t1 t2
evalSmallStep t = t

eval :: Term -> Term
eval t = case evalSmallStep t of
  (t1' :@ t2') -> eval (t1' :@ t2')
  t -> t

run :: Term -> (Context, Term)
run term = (doType emptyContext term', term')
  where
    context = doType emptyContext term
    term'   = case context of
                TypeErrorContext _ -> term
                _ -> eval term

runWithContext :: Show lineId => (lineId, Term) -> IO ()
runWithContext (lineId, term) =
  let (context, term') = run term
      typ = lookupType term' context
  in
    putStr $
      "\n\n-- " ++ (show lineId) ++ ": " ++ (show term) ++ "\n\n" ++
      case typ of
        Nothing -> "Couldn't be typed: " ++ (show term')
        Just (TypeError terror) -> "Couldn't be typed: "
        Just typ' ->  showTermType (term', typ')
      ++ "\n" ++
      (show context)

main = do
  undefined
  --mapM_ runWithContext
  --  [ ( 1, Var "x") -- error: x hasn't been declared
  --  , ( 2, intIdentity)
  --  , ( 3, Λ (Param "y" int) (intIdentity :@ (Var "y")))
  --  , ( 4, intConstant)
  --  , ( 5, fnfn)
  --  , ( 6, fnfn :@ (Var "a")) -- error
  --  , ( 7, fnfn :@ (Λ (Param "a" (Type TBool)) (Var "a"))) -- error
  --  , ( 8, intIdentity :@ (VInt 4))
  --  , (10, intHead :@ (intCons :@ (VInt 3) :@ (VInt 2)))
  --  ]
