module Main where

import SimplyTyped hiding (main)
import Types
import Typing
import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

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
  , testProperty "identity function (for integers) returns argument"
                 prop_evalIntIdentity
  ]

examples :: Test
examples = testGroup "simply typed lambda calculus: examples" []

main :: IO ()
main = defaultMain [properties, examples]
