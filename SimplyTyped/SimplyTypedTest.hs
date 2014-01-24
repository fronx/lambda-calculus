module Main where

import SimplyTyped hiding (main)
import SimplyTypedLib
import Types
import Typing
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

genTypeId :: Gen TypeId
genTypeId = elements [ TInt, TBool ]

genSimpleType :: Gen Type
genSimpleType = liftM Type genTypeId

genType :: Gen Type
genType = sized genTypeN
  where genTypeN 0 = genSimpleType
        genTypeN n = liftM2 (:->) (genTypeN (n `div` 2))
                                  (genTypeN (n `div` 2))

genParam :: Gen Param
genParam = liftM2 Param genVarname genType

genParamWithType :: Gen Type -> Gen Param
genParamWithType genT = do
  typ <- genT
  vname <- genVarname
  return $ Param vname typ

genTermVar,
  genTermLambda,
  genTermApp :: Gen Term

genTermVar    = liftM Var genVarname
genTermLambda = liftM2 Λ arbitrary arbitrary
genUnaryFunction = liftM2 Λ (genParamWithType genSimpleType) arbitrary
genTermApp    = liftM2 (:@) arbitrary arbitrary
genTermInt    = liftM VInt arbitrary
genTermBool   = liftM VBool arbitrary

genTerm = oneof
  [ genTermVar
  , genTermLambda
  , genTermApp
  , genTermInt
  , genTermBool
  ]

instance Arbitrary Param where
  arbitrary = genParam

instance Arbitrary Term where
  arbitrary = genTerm

prop_evalIntIdentity :: Term -> Property
prop_evalIntIdentity term = property $
  evalSmallStep (intIdentity :@ term) == term

prop_WellTypedCanBeEvaluated :: Term -> Property
prop_WellTypedCanBeEvaluated term =
  (isWellTyped term) ==> isWellTyped (eval term)

prop_dontEvalLambdas :: Property
prop_dontEvalLambdas = forAll genTermLambda $
  \term -> eval term == term

properties :: Test
properties = testGroup "simply typed lambda calculus: properties" $
  [ testProperty "well-typed expressions can be evaluated"
                 prop_WellTypedCanBeEvaluated
  , testProperty "lambda abstractions are not evaluated"
                 prop_dontEvalLambdas
  , testProperty "identity function (for integers) returns its argument"
                 prop_evalIntIdentity
  ]

testOverloadedParameter :: Assertion
testOverloadedParameter =
  (eval $ f :@ (VInt 1) :@ (VInt 2)) @?=
    (VInt 2)
  where x = Param "x" (Type TInt)
        f = Λ x (Λ x (Var "x"))

testOverloadedParameter' :: Assertion
testOverloadedParameter' =
  (eval $ f :@ (VInt 1) :@ (VInt 2) :@ (VInt 3)) @?=
    (VInt 3)
  where x = Param "x" (Type TInt)
        y = Param "y" (Type TInt)
        f = Λ x (Λ y (Λ x (Var "x")))

testApply :: Assertion
testApply =
  apply f (VInt 1) @?=
    Λ y (VInt 1)
  where x = Param "x" (Type TInt)
        y = Param "y" (Type TInt)
        f = Λ x (Λ y (Var "x"))

  --  , ( 9, fnfn :@ (Λ (Param "a" int) (Var "a")))

  --let fnfn = Λ (Param "f" (int :-> int))
  --             (Λ (Param "x" int)
  --               ((Var "f") :@ ((Var "f") :@ (Var "x"))))

examples :: Test
examples = testGroup "simply typed lambda calculus: examples" $
  [ testCase "beta reduction with overloaded parameter"
             testOverloadedParameter
  , testCase "beta reduction with overloaded parameter (2)"
             testOverloadedParameter'
  , testCase "beta reduction, simple case"
             testApply
  ]

main :: IO ()
main = defaultMain [properties, examples]
