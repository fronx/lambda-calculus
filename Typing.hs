module Typing where

import Prelude hiding (fail)
import Types
import Data.Maybe (isJust, fromJust)

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
doType context (Var vname) =
  contextPush context (term, typ)
  where
    term = Var vname
    typ  = lookupTypeOrTypeError term context
doType context (Λ (Param pname ptype) body) =
  case lookupType var context of
    Just typ -> -- param overloads existing var
      contextPush context' (term, TypeError terror')
      where
        context' = contextPush context (var, TypeError terror)
        terror   = FailDoTypeVar (Param pname ptype)
        terror'  = FailDueToPreviousTypeError term terror
    Nothing -> -- param uses a fresh name
      contextPush context' (term, typ)
      where
        typ        = ptype :-> lookupType' body context''
        context''  = doType context' body
        context'   = contextPush context (var, ptype)
  where
    term = (Λ (Param pname ptype) body)
    var = Var pname
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
