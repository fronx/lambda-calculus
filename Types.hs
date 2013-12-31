module Types where

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
