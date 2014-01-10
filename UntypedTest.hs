module UntypedTest where

import Untyped hiding (main)
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

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
genTermLambda = liftM2 Λ genParam arbitrary
genTermApp    = liftM2 (:@) arbitrary arbitrary
genTermAtom   = liftM Atom arbitrary

genTerm = do
  n <- choose (0, 3) :: Gen Int
  case n of
    0 -> genTermVar
    1 -> genTermLambda
    2 -> genTermApp
    3 -> genTermAtom

instance Arbitrary Param where
  arbitrary = genParam

instance Arbitrary Term where
  arbitrary = genTerm

prop_evalIdentity :: Term -> Bool
prop_evalIdentity term =
  evalSmallStep (identity :@ term) == term

prop_evalConstant :: Term -> Bool
prop_evalConstant term =
  evalSmallStep (constant :@ term) == Var "y"

main = do
  quickCheck prop_evalIdentity
  quickCheck prop_evalConstant
