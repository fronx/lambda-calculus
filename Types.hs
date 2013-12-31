module Types where

import Data.List (intercalate)

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

data Context = Context [ (Term, Type) ]

emptyContext = Context []

contextPush :: Context -> (Term, Type) -> Context
contextPush (Context items) item = Context (item : items)

instance Show Context where
  show (Context items) = "\n[\n    " ++ intercalate "\n\n    " (map (\(a, b) -> (show a) ++ " :: \n    " ++ show b) items) ++ "\n]\n"
