module SimplyTypedTest where

import SimplyTyped hiding (main)
import Types
import Typing
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

singleCharStrings chars =
  (concat . map (\c -> [[c]])) chars

genVarname :: Gen String
genVarname = elements $ singleCharStrings "abcdef"

genTypeId :: Gen TypeId
genTypeId = elements [ TInt, TBool ]

genSimpleType :: Gen Type
genSimpleType = liftM Type genTypeId

genUnaryFunctionType :: Gen Type
genUnaryFunctionType = liftM2 (:->) genSimpleType genSimpleType

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
genFnAB       = liftM2 Λ (genParamWithType genUnaryFunctionType) arbitrary
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
  printTestCase "well-typed expressions can be evaluated." $
    (isWellTyped term) ==> isWellTyped (eval term)

prop_dontEvalLambdas :: Property
prop_dontEvalLambdas =
  printTestCase "lambda abstractions are not evaluated." $
    forAll genTermLambda $
      \term -> eval term == term

main = do
  quickCheck prop_evalIntIdentity
  quickCheck prop_WellTypedCanBeEvaluated
  quickCheck prop_dontEvalLambdas
