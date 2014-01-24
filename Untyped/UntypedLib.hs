module UntypedLib where

import Untyped

identity = Λ (Param "x") (Var "x")
constant = Λ (Param "x") (Var "y")

fnfn = Λ (Param "f")
        (Λ (Param "x")
          ((Var "f") :@ ((Var "f") :@ (Var "x"))))

cons = Λ (Param "a")
        (Λ (Param "b")
          (Λ (Param "f")
            ((Var "f") :@ (Var "a") :@ (Var "b"))))

first  = Λ (Param "a") (Λ (Param "b") (Var "a"))
second = Λ (Param "a") (Λ (Param "b") (Var "b"))

lhead = Λ (Param "c") (Var "c" :@ first)
ltail = Λ (Param "c") (Var "c" :@ second)
