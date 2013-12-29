type VarName  = String
type TypeName = String

data Type = Type TypeName
          | Type :-> Type
          deriving (Show, Eq)

data TypedParam = Param VarName Type
                deriving (Show, Eq)

data Term = Var VarName
          | Λ TypedParam Term
          | Term :@ Term
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
replace a b (t1 :@ t2) = undefined

apply :: Term -> Term -> Term
apply (Λ (Param pname ptype) body)
      (Var vname) = replace (Var pname)
                            (Var vname) body

eval :: Term -> Term
eval (t1 :@ t2) = apply t1 t2
eval t = t

---

main = do
  let int = Type "Int"
  let x = Param "x" int
  let term1 = Λ x (Var "x")
  let term2 = Λ x (Var "y")
  let app1 = term1 :@ (Var "a")
  let app2 = term2 :@ (Var "a")

  let context1 = C0 :<< Param "x" (Type "Int")
                    :<< Param "y" (Type "Int")
  print int
  print x
  print term1
  print term2
  print context1
  print "---"
  print $ eval term1 -- Λ (Param "x" (Type "Int")) (Var "x")
  print $ eval app1  -- Var "a"
  print $ eval app2  -- Var "y"
