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
doType context (VInt i) =
  contextPush context (VInt i, Type TInt)
doType context (VBool b) =
  contextPush context (VBool b, Type TBool)
doType context (Var vname) =
  contextPush context (term, typ)
  where
    term = Var vname
    typ  = lookupTypeOrTypeError term context
doType context (Λ (Param pname ptype) body) =
  contextPush context' (term, typ)
  where
    term = Λ (Param pname ptype) body
    typ = case lookupType' body context'' of
            TypeError terror -> TypeError $ FailDueToPreviousTypeError body terror
            resultType       -> ptype :-> resultType
    context''  = doType context' body
    context'   = contextPush context ((Var pname), ptype)
doType context (term1 :@ term2) =
  contextPush context' (term, typ)
  where
    term     = term1 :@ term2
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
