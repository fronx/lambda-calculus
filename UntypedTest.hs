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

main = do
  mapM quickCheck -- verboseCheck
    [ prop_evalIdentity
    , prop_evalConstant
    , prop_dontEvalLambdas
    ]
