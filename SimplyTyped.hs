module SimplyTyped where

import Data.List

type VarName  = String
type TypeName = String

data Error = Error String

data Type = Type TypeName
          | Type :-> Type
          deriving (Show, Eq)

data Param = Param VarName Type
           deriving (Show, Eq)

data Term = Var VarName
          | Λ Param Term
          | Apply Term Term
          deriving (Show, Eq)

type Context = [ (Term, Type) ]

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

apply :: Term -> Term -> Either String Term
apply (Λ (Param pname ptype) body) term
      = Right $ replace (Var pname) term body
apply a b
      = Left $ "Can't apply " ++ (show a) ++ " to " ++ (show b)

eval :: Term -> Either String Term
eval (Apply t1 t2) = apply t1 t2
eval t = Right t

evalAndPrint :: Term -> IO ()
evalAndPrint t =
  case eval t of
    Left msg -> putStrLn msg
    Right t' -> putStrLn (show t')

lookupType :: Term -> Context -> Either String Type
lookupType term context =
  case lookup term context of
    Just _type -> Right _type
    Nothing    -> Left $ "Type of term " ++ (show term) ++ " unknown at the point of it being queried."

eitherToBool :: Either a b -> Bool
eitherToBool x =
  case x of
    Right _ -> True
    Left  _ -> False

typeKnown :: Term -> Context -> Bool
typeKnown = (.) eitherToBool . lookupType

doType :: Context -> Term -> Either String Context
doType context (Var vname) =
  case lookupType (Var vname) context of
    Left msg    -> Left msg
    Right _type -> Right $ context
doType context (Λ (Param pname ptype) body) =
  if typeKnown (Var pname) context
    then
      Left $ "Variable names can't be overloaded: " ++ (show ptype)
    else
      let context' = doType ((Var pname, ptype) : context) body
          lambdaTerm = (Λ (Param pname ptype) body)
      in case context' of
           Left msg       -> Left msg
           Right _context ->
             case lookupType body _context of
               Left msg -> Left msg
               Right resulttype -> Right $ (lambdaTerm, ptype :-> resulttype) : _context

---

main = do
  let int = Type "int"
  let x = Param "x" int
  let f = Param "f" int

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

  print $ doType [] (Var "x") -- error: x hasn't been declared
  print $ doType [] identity
  print $ doType [] other
