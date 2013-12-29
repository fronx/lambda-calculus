type VarName  = String
type TypeName = String

data Type = Type TypeName
          | Type :-> Type
          deriving (Show, Eq)

data TypedParam = Param VarName Type
                deriving (Show, Eq)

data Term = Var VarName
          | Λ TypedParam Term
          | Apply Term Term -- issue: the first term must be a lambda expression.
          deriving (Show, Eq)

data Context = C0
             | Context :<< TypedParam
             deriving Show

replace :: Term -- old
        -> Term -- new
        -> Term -- term
        -> Term -- result
replace a b (Var x)
  | a == (Var x) = b
  | otherwise    = Var x
replace a b (Λ typedParam term)
  | a == term = Λ typedParam b
  | otherwise = Λ typedParam (replace a b term)
replace a b (Apply t1 t2) = Apply (replace a b t1) (replace a b t2)

apply :: Term -> Term -> Term
apply (Λ (Param pname ptype) body)
      term
      = replace (Var pname) term body
apply _ _ = error "first parameter must be a lambda expression"

eval :: Term -> Term
eval (Apply t1 t2) = apply t1 t2
eval t = t

---

main = do
  let int = Type "Int"
  let x = Param "x" int
  let f = Param "f" (int :-> int)

  let identity = Λ x (Var "x")
  let other    = Λ x (Var "y")

  let context1 = C0 :<< Param "x" (Type "Int")
                    :<< Param "y" (Type "Int")

  let fnfn = Λ f
               (Λ x (Apply (Var "f")
                           (Apply (Var "f")
                                  (Var "x"))))
  print fnfn

  print "---"
  print $ eval identity -- shouldn't change anything
  print $ eval $
    Apply identity (Var "a") -- Var "a"
  print $ eval $
    Apply other (Var "a") -- other
  print $ eval $
    Apply identity other -- other
  print $ eval $
    Apply identity other -- other
  print $ eval $
    Apply fnfn (Var "a") -- the result of this is meaningless :(
