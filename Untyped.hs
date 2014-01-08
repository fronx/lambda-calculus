module Untyped where

data Error = Error String

type VarName = String

data Param = Param VarName
           deriving (Show, Eq)

data Term = Var VarName
          | Λ Param Term
          | Apply Term Term
          deriving (Show, Eq)

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
apply (Λ (Param pname) body) term =
  replace (Var pname) term body
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

eval :: Term -> Term
eval (Apply t1 t2) = apply t1 t2
eval (Λ param body) = Λ param (eval body) -- FIXME
eval t = t

run :: Term -> IO ()
run t = print (eval t)


main = do
  let x = Param "x"
  let f = Param "f"

  let identity = Λ x (Var "x")
  let other    = Λ x (Var "y")

  let fnfn = Λ f
               (Λ x (Apply (Var "f")
                           (Apply (Var "f")
                                  (Var "x"))))
  print fnfn

  mapM run
    [ identity                 -- shouldn't change anything
    , Apply identity (Var "a") -- Var "a"
    , Apply other (Var "a")    -- other
    , Apply identity other     -- other
    , Apply fnfn (Var "a")     -- the result of this is meaningless
    , Λ (Param "y") (Apply identity (Var "y"))
    , fnfn -- FIXME
    ]
