module Untyped where

data Error = Error String

type VarName = String

data Param = Param VarName
           deriving (Show, Eq)

data Term = Var VarName
          | Λ Param Term
          | Term :@ Term
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
replace a b (t1 :@ t2) = (replace a b t1) :@ (replace a b t2)

apply :: Term -> Term -> Term
apply (Λ (Param pname) body) term =
  replace (Var pname) term body
apply a b = error $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

eval :: Term -> Term
eval (t1 :@ t2) = apply t1 t2
eval t = t

run :: Term -> IO ()
run t = print (eval t)


main = do
  let identity = Λ (Param "x") (Var "x")
  let other    = Λ (Param "x") (Var "y")

  let fnfn = Λ (Param "f") (Λ (Param "x") ((Var "f") :@ ((Var "f") :@ (Var "x"))))

  let cons = Λ (Param "a")
               (Λ (Param "b")
                 (Λ (Param "f")
                   ((Var "f") :@ (Var "a") :@ (Var "b"))))

  mapM run
    [ identity              -- shouldn't change anything
    , identity :@ (Var "a") -- Var "a"
    , other :@ (Var "a")    -- other
    , identity :@ other     -- other
    , fnfn :@ (Var "a")     -- the result of this is meaningless
    , Λ (Param "y") (identity :@ (Var "y"))
    , fnfn
    ]
