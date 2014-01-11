module SimplyTypedTest where

import SimplyTyped hiding (main)
import Types
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

singleCharStrings chars =
  (concat . map (\c -> [[c]])) chars

genVarname :: Gen String
genVarname = elements $ singleCharStrings "abcdef"

genTypeId :: Gen TypeId
genTypeId = elements [ TInt, TBool ]

genType :: Gen Type
genType = oneof
  [ liftM Type genTypeId
  , liftM2 (:->) genType genType
  ]

genParam :: Gen Param
genParam = liftM2 Param genVarname genType

genTermVar,
  genTermLambda,
  genTermApp :: Gen Term

genTermVar    = liftM Var genVarname
genTermLambda = liftM2 Λ arbitrary arbitrary
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

prop_dontEvalLambdas :: Term -> Property
prop_dontEvalLambdas term = forAll genTermLambda $
  \term -> eval term == term

main = do
   mapM quickCheck
    [ prop_evalIntIdentity
    , prop_WellTypedCanBeEvaluated
    , prop_dontEvalLambdas
    ]
    --, prop_evalConstant
    --, prop_dontEvalLambdas
    --]
