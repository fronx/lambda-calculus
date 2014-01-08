module Typing where

import Prelude hiding (fail)
import Types
import Data.Maybe (isJust, fromJust)

lookupType :: Term -> Context -> Maybe Type
lookupType term (Context items)          = lookup term items
lookupType term (TypeErrorContext items) = lookup term items

lookupType' :: Term -> Context -> Type
lookupType' = (.) fromJust . lookupType

doType :: Context -> Term -> Context
doType context (Var vname) =
  contextPush context (term, typ)
  where term = Var vname
        typ = case lookupType term context of
                Nothing   -> TypeError (FailLookupType term)
                Just typ' -> typ'
doType context (Λ (Param pname ptype) body) =
  case lookupType (Var pname) context of
    Just typ ->
      let context' = contextPush context (Var pname, TypeError terror)
          terror = FailDoTypeVar (Param pname ptype)
          term = (Λ (Param pname ptype) body)
      in contextPush context' (term, TypeError (FailDueToPreviousTypeError term terror))
    Nothing ->
      let context' = doType (contextPush context (Var pname, ptype)) body
          lambdaTerm = (Λ (Param pname ptype) body)
          resulttype = lookupType' body context'
      in contextPush context' (lambdaTerm, ptype :-> resulttype)
doType context (Apply term1 term2) =
  contextPush context' (term, typ)
  where
    context' = doType (doType context term1) term2
    typeT1 = lookupType' term1 context'
    typeT2 = lookupType' term2 context'
    term   = (Apply term1 term2)
    typ = case typeT1 of
      TypeError terror ->
        TypeError (FailDueToPreviousTypeError term terror)
      a :-> b ->
        case typeT2 of
          TypeError terror -> TypeError (FailDueToPreviousTypeError term terror)
          typ2 ->
            if a /= typ2
            then
              TypeError (FailDoTypeApplyBadArg term2 a typ2)
            else
              b
      notAFunctionType ->
        TypeError (FailDoTypeApplyT1Fn term1 typeT1)
