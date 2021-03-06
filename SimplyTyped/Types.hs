module Types where

import Data.List (intercalate, nub)

type VarName  = String
data TypeId = TInt
            | TBool
            | TList TypeId
            deriving (Show, Eq)

data Error = Error String

data TypeError = FailLookupType        Term
               | FailDoTypeVar         Param
               | FailDoTypeApplyT1Fn   Term
                                       Type
               | FailDoTypeApplyBadArg Term
                                       Type -- expected
                                       Type -- actual
               | FailDueToPreviousTypeError Term
                                            TypeError -- previous
               deriving (Eq)

instance Show TypeError where
  show (FailLookupType term) =
    "Type of term " ++ (show term) ++ " unknown."
  show (FailDoTypeVar param) =
    "Variable names can't be overloaded: " ++ (show param)
  show (FailDoTypeApplyT1Fn term typ) =
    "Application not possible. Term " ++ (show term) ++
    " was expected to be a function, but its type is " ++ (show typ) ++ "."
  show (FailDoTypeApplyBadArg term texpected tactual) =
    "Incompatible argument: " ++ (show term) ++ "\n" ++
    "  Expected: " ++ (show texpected) ++ "\n" ++
    "  Received: " ++ (show tactual)
  show (FailDueToPreviousTypeError term terror) =
    "Term " ++ (show term) ++
    " can't be typed due to a previous type error:\n    " ++ (show terror)

data Type = Type TypeId
          | Type :-> Type
          | TypeError TypeError
          deriving (Show, Eq)

infixr 5 :-> -- right-associative

data Param = Param VarName Type
           deriving (Show, Eq)

data Term = Var VarName
          | Λ Param Term
          | Term :@ Term
          | VInt Int
          | VBool Bool
          | VNilInt
          deriving (Show, Eq)

-- TODO use a map instead
data Context = Context [ (Term, Type) ]
             | TypeErrorContext [ (Term, Type) ]
             deriving Eq

emptyContext :: Context
emptyContext = Context []

contextPush :: Context -> (Term, Type) -> Context
contextPush (TypeErrorContext items) tt =
  TypeErrorContext $ nub (tt : items)
contextPush (Context items) tt
  | (term, TypeError terror) <- tt
  = TypeErrorContext $ nub (tt : items)
  | otherwise = Context $ nub (tt : items)

isTypeError :: (Term, Type) -> Bool
isTypeError (_, TypeError te) = True
isTypeError (_, _) = False

instance Show Context where
  show (Context items) = showContextItems items
  show (TypeErrorContext items) =
    showContextItems items ++
      concat (map showType (filter isTypeError (reverse items)))
    where showType (term, typ) = (show typ) ++ "\n"

showTermType (term, typ) =
  (show term) ++ " :: \n    " ++ (show typ)

showContextItems items =
  "[\n    " ++
  intercalate "\n\n    " (map showTermType items) ++
  "\n]\n"
