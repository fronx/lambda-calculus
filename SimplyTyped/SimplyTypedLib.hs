module SimplyTypedLib
  ( int
  , intIdentity
  , intConstant
  , intCons
  , intFirst
  , intSecond
  , intHead
  , intTail
  )
where

import SimplyTyped

int = Type TInt

intIdentity = Λ (Param "x" int) (Var "x")
intConstant = Λ (Param "x" int) (Var "y")

intList = Type (TList TInt)
intConsList = int :-> intList :-> intList

intCons = Λ (Param "a" int)
           (Λ (Param "b" intList)
             (Λ (Param "f" intConsList)
               ((Var "f") :@ (Var "a") :@ (Var "b"))))

intFirst  = Λ (Param "a" int) (Λ (Param "b" intList) (Var "a"))
intFirstList = Λ (Param "a" int) (Λ (Param "b" intList)
  (intCons :@ (Var "a") :@ VNilInt))
intSecond = Λ (Param "a" int) (Λ (Param "b" intList) (Var "b"))

intHead =
  Λ (Param "c" (intConsList :-> intList))
    (Var "c" :@ intFirstList)
intTail =
  Λ (Param "c" (intConsList :-> intList))
    (Var "c" :@ intSecond)
