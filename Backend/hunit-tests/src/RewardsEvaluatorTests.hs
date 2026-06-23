{-# LANGUAGE OverloadedStrings #-}

module RewardsEvaluatorTests (tests) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Types.RewardCohort as DRC
import Kernel.Prelude
import Kernel.Types.Id
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "RewardsEvaluator"
    [ testCase "ridesLast7d >= 5 matches when rider has 5 rides in window" $ do
        let ctx = mkCtx "ridesLast7d" 5
            cohort = mkCohort "c1" (gteRule "ridesLast7d" 5)
        matched <- Eval.evaluateCohortsPure ctx [cohort]
        matched @?= [Id "c1"],
      testCase "ridesLast7d >= 5 does not match when rider has 4 rides" $ do
        let ctx = mkCtx "ridesLast7d" 4
            cohort = mkCohort "c1" (gteRule "ridesLast7d" 5)
        matched <- Eval.evaluateCohortsPure ctx [cohort]
        matched @?= [],
      testCase "multiple cohorts: only matching ones returned" $ do
        let ctx = mkCtx "ridesLast7d" 10
            c1 = mkCohort "c1" (gteRule "ridesLast7d" 5)
            c2 = mkCohort "c2" (gteRule "ridesLast7d" 50)
            c3 = mkCohort "c3" (gteRule "ridesLast7d" 10)
        matched <- Eval.evaluateCohortsPure ctx [c1, c2, c3]
        matched @?= [Id "c1", Id "c3"]
    ]

mkCtx :: Text -> Int -> A.Value
mkCtx field n = A.Object $ KM.fromList [(field, A.toJSON n)]

gteRule :: Text -> Int -> A.Value
gteRule field n =
  A.Object $
    KM.fromList
      [ (">=", A.Array $ fromList [A.Object $ KM.fromList [("var", A.String field)], A.toJSON n])
      ]

mkCohort :: Text -> A.Value -> DRC.RewardCohort
mkCohort cohortId rule =
  DRC.RewardCohort
    { id = Id cohortId,
      campaignId = Id "test-campaign",
      name = "test",
      description = Nothing,
      displayOrder = 0,
      eligibilityJsonLogic = rule,
      rewardTitle = "test",
      rewardImageUrl = Nothing,
      couponValidityDays = Nothing,
      createdAt = read "2026-01-01 00:00:00 UTC",
      updatedAt = read "2026-01-01 00:00:00 UTC"
    }
