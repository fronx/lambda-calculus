module Typing where

import Types
import Data.Maybe (fromJust)

lookupType :: Term -> Context -> Maybe Type
lookupType term (Context items)          = lookup term items
lookupType term (TypeErrorContext items) = lookup term items

lookupType' :: Term -> Context -> Type
lookupType' = (.) fromJust . lookupType

lookupTypeOrTypeError :: Term -> Context -> Type
lookupTypeOrTypeError term context =
  case lookupType term context of
    Nothing   -> TypeError (FailLookupType term)
    Just typ' -> typ'

doType :: Context -> Term -> Context
doType context term
  | VInt  _ <- term = contextPush context (term, Type TInt)
  | VBool _ <- term = contextPush context (term, Type TBool)
  | Var   _ <- term = contextPush context (term, lookupTypeOrTypeError term context)
  | Λ (Param pname ptype) body <- term
    = let typ = case lookupType' body context'' of
                  TypeError terror -> TypeError $ FailDueToPreviousTypeError body terror
                  resultType       -> ptype :-> resultType
          context''  = doType context' body
          context'   = contextPush context ((Var pname), ptype)
      in contextPush context' (term, typ)
  | (term1 :@ term2) <- term
    = let
        context' = doType (doType context term1) term2
        typeT1   = lookupType' term1 context'
        typeT2   = lookupType' term2 context'
        typ = case typeT1 of
          TypeError terror ->
            TypeError (FailDueToPreviousTypeError term terror)
          a :-> b ->
            case typeT2 of
              TypeError terror -> TypeError (FailDueToPreviousTypeError term terror)
              typ2 ->
                if a /= typ2 then
                  TypeError (FailDoTypeApplyBadArg term2 a typ2)
                else
                  b
          notAFunctionType ->
            TypeError (FailDoTypeApplyT1Fn term1 typeT1)
      in contextPush context' (term, typ)

isWellTyped :: Term -> Bool
isWellTyped term =
  case doType emptyContext term of
    TypeErrorContext _ -> False
    _ -> True

computeType :: Term -> Type
computeType term =
  lookupType' term context
  where context = doType emptyContext term
