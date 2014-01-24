module Main where

import Untyped hiding (main)
import UntypedLib
import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.HUnit (Assertion, (@?=))
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

singleCharStrings chars =
  (concat . map (\c -> [[c]])) chars

genVarname :: Gen String
genVarname = elements $ singleCharStrings "abcdef"

genParam :: Gen Param
genParam = liftM Param genVarname

genTermVar,
  genTermLambda,
  genTermApp,
  genTermAtom :: Gen Term

genTermVar    = liftM Var genVarname
genTermLambda = liftM2 Λ arbitrary arbitrary
genTermApp    = liftM2 (:@) arbitrary arbitrary
genTermAtom   = liftM Atom arbitrary

genTerm = oneof
  [ genTermVar
  , genTermLambda
  , genTermApp
  , genTermAtom
  ]

instance Arbitrary Param where
  arbitrary = genParam

instance Arbitrary Term where
  arbitrary = genTerm

prop_evalIdentity :: Term -> Property
prop_evalIdentity term = property $
  evalSmallStep (identity :@ term) == term

prop_evalConstant :: Term -> Property
prop_evalConstant term = property $
  evalSmallStep (constant :@ term) == Var "y"

prop_dontEvalLambdas :: Term -> Property
prop_dontEvalLambdas term = forAll genTermLambda $
  \term -> eval term == term

properties :: Test
properties = testGroup "untyped lambda calculus: properties" $
  [ testProperty "identity function returns its argument"
                 prop_evalIdentity
  , testProperty "constant function returns its body"
                 prop_evalConstant
  , testProperty "lambda abstractions are not evaluated"
                 prop_dontEvalLambdas
  ]

testListHead :: Assertion
testListHead =
  (eval (lhead :@ (cons :@ (Atom 3) :@ (Atom 0)))) @?=
    (Atom 3)

testListTail :: Assertion
testListTail =
  (eval (ltail :@ (cons :@ (Atom 3) :@ (Atom 0)))) @?=
    (Atom 0)

testListTail' :: Assertion
testListTail' =
  (eval (ltail :@ (cons :@ (Atom 3) :@ (cons :@ (Atom 2) :@ (Atom 0))))) @?=
    (eval (cons :@ (Atom 2) :@ (Atom 0)))

testListTail'' :: Assertion
testListTail'' =
  (eval (ltail :@ (ltail :@ (cons :@ (Atom 3) :@ (cons :@ (Atom 2) :@ (Atom 0)))))) @?=
    (Atom 0)

--Λ (Param "f") (
--  (Λ (Param "a") (
--    Λ (Param "b") (Atom 0)
--  ) :@ Atom 2) :@ Atom 0)

examples :: Test
examples = testGroup "untyped lambda calculus: examples" $
  [ testCase "list ops: head" testListHead
  , testCase "list ops: tail (return one item)"  testListTail
  , testCase "list ops: tail (return two items)" testListTail'
  , testCase "list ops: tail twice"              testListTail''
  ]

main :: IO ()
main = defaultMain [properties, examples]
