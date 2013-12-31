module Typing where

import Prelude hiding (fail)
import Types
import Ether

failLookupType :: Term -> String
failLookupType term = "Type of term " ++ (show term) ++ " unknown."

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

doType :: Context -> Term -> Either String Context
doType context (Var vname) =
  ifRight
    (lookupType (Var vname) context)
    (\x -> Right context)
doType context (Λ (Param pname ptype) body) =
  if typeKnown (Var pname) context
    then
      fail $ failDoTypeVar (Param pname ptype)
    else
      let context' = doType ((Var pname, ptype) : context) body
          lambdaTerm = (Λ (Param pname ptype) body)
      in case context' of
        Left msg -> fail msg
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
          fail $ failDoTypeApplyBadArg term2 argtypeT1 typeT2
        else
          case restType typeT1 of
            Nothing -> fail $ failDoTypeApplyT1Fn term1 typeT1
            Just restTypeT1 ->
              Right $ ((Apply term1 term2), restTypeT1)
                    : (term1, typeT1)
                    : (term2, typeT2)
                    : conT2)
