module SimplyTyped where

import Data.List
import Ether

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

failApply :: Term -> Term -> String
failApply a b = "Can't apply " ++ (show a) ++ " to " ++ (show b)

apply :: Term -> Term -> Either String Term
apply (Λ (Param pname ptype) body) term
      = Right $ replace (Var pname) term body
apply a b
      = Left $ failApply a b

eval :: Term -> Either String Term
eval (Apply t1 t2) = apply t1 t2
eval t = Right t

evalAndPrint :: Term -> IO ()
evalAndPrint t =
  case eval t of
    Left msg -> putStrLn msg
    Right t' -> putStrLn (show t')

failLookupType :: Term -> String
failLookupType term = "Type of term " ++ (show term) ++ " unknown."

lookupType :: Term -> Context -> Either String Type
lookupType term context =
  maybeToEither (lookup term context) (failLookupType term)

-- force!
lookupType' :: Term -> Context -> Type
lookupType' = (.) forceRight . lookupType

typeKnown :: Term -> Context -> Bool
typeKnown = (.) eitherToBool . lookupType

argType :: Type -> Type
argType (first :-> rest) = first
argType t = t

restType :: Type -> Maybe Type
restType (param :-> rest) = Just rest
restType _ = Nothing

failDoTypeVar :: Param -> String
failDoTypeVar param = "Variable names can't be overloaded: " ++ (show param)

failDoTypeApplyT1Fn :: Term -> Type -> String
failDoTypeApplyT1Fn term typ =
  "Application not possible. Term " ++ (show term) ++
  " was expected to be a function, but its type is " ++ (show typ) ++ "."

failDoTypeApplyBadArg :: Term -> Type -> Type -> String
failDoTypeApplyBadArg term texpected tactual =
  "Incompatible argument: " ++ (show term) ++
  " Expected: " ++ (show texpected) ++
  " Received: " ++ (show tactual)

doType :: Context -> Term -> Either String Context
doType context (Var vname) =
  case lookupType (Var vname) context of
    Left msg    -> Left msg
    Right _type -> Right $ context
doType context (Λ (Param pname ptype) body) =
  if typeKnown (Var pname) context
    then
      Left $ failDoTypeVar (Param pname ptype)
    else
      let context' = doType ((Var pname, ptype) : context) body
          lambdaTerm = (Λ (Param pname ptype) body)
      in case context' of
           Left msg       -> Left msg
           Right _context ->
             Right $ (lambdaTerm, ptype :-> resulttype) : _context
             where resulttype = lookupType' body _context
doType context (Apply term1 term2) =
  ifRight2
    (doType context term1)
    (doType context term2)
    (\conT1 conT2 ->
      let typeT1 = lookupType' term1 conT1
          typeT2 = lookupType' term2 conT2
          argtypeT1 = argType typeT1
      in if argtypeT1 /= typeT2
           then
             Left $ failDoTypeApplyBadArg term2 argtypeT1 typeT2
           else
             case restType typeT1 of
               Nothing -> Left $ failDoTypeApplyT1Fn term1 typeT1
               Just restTypeT1 -> Right $
                   ((Apply term1 term2), restTypeT1)
                 : (term1, typeT1)
                 : (term2, typeT2)
                 : conT2)

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

  print $ doType [] (Var "x") -- error: x hasn't been declared
  print $ doType [] identity
  print $ doType [] other
  print $ doType [] fnfn
