{-# LANGUAGE OverloadedStrings #-}

module RewardsEvaluatorTests (tests) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import Domain.Types.RewardContext (RewardContext (..), defaultRewardContext, rewardContextKeys, rewardContextToLogicInput)
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Queries.RewardUnlockExtra as QRUE
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
        matched @?= [Id "c1", Id "c3"],
      -- interpretEligibility: the shared truthiness rule
      testCase "interpretEligibility: true boolean and non-zero number are eligible" $ do
        Eval.interpretEligibility (A.Bool True) @?= True
        Eval.interpretEligibility (A.Number 3) @?= True,
      testCase "interpretEligibility: false / zero / null / other are not eligible" $ do
        Eval.interpretEligibility (A.Bool False) @?= False
        Eval.interpretEligibility (A.Number 0) @?= False
        Eval.interpretEligibility A.Null @?= False
        Eval.interpretEligibility (A.String "nope") @?= False,
      -- evalCohortLogic: same verdict as production, run against a typed context
      testCase "evalCohortLogic: eligible when ridesLast7d >= threshold" $ do
        let ctx = rewardContextToLogicInput defaultRewardContext {ridesLast7d = Just 6}
        Eval.evalCohortLogic (gteRule "ridesLast7d" 5) ctx @?= Right (A.Bool True, True),
      testCase "evalCohortLogic: not eligible when ridesLast7d < threshold" $ do
        let ctx = rewardContextToLogicInput defaultRewardContext {ridesLast7d = Just 4}
        Eval.evalCohortLogic (gteRule "ridesLast7d" 5) ctx @?= Right (A.Bool False, False),
      -- rewardContextToLogicInput: production output must not change (regression)
      testCase "rewardContextToLogicInput: all-Just reproduces the legacy context object" $ do
        let ctx =
              RewardContext
                { ridesLast1d = Just 1,
                  ridesLast3d = Just 2,
                  ridesLast7d = Just 3,
                  ridesLast30d = Just 4,
                  ridesLast90d = Just 5,
                  hasTakenValidRide = Just True,
                  isValidRide = Just False
                }
            expected =
              A.object
                [ "ridesLast1d" A..= (1 :: Int),
                  "ridesLast3d" A..= (2 :: Int),
                  "ridesLast7d" A..= (3 :: Int),
                  "ridesLast30d" A..= (4 :: Int),
                  "ridesLast90d" A..= (5 :: Int),
                  "hasTakenValidRide" A..= True,
                  "isValidRide" A..= Just False
                ]
        rewardContextToLogicInput ctx @?= expected,
      -- rewardContextToLogicInput: absent fields default so a logic still runs
      testCase "rewardContextToLogicInput: empty context defaults counts to 0, isValidRide null" $ do
        let expected =
              A.object
                [ "ridesLast1d" A..= (0 :: Int),
                  "ridesLast3d" A..= (0 :: Int),
                  "ridesLast7d" A..= (0 :: Int),
                  "ridesLast30d" A..= (0 :: Int),
                  "ridesLast90d" A..= (0 :: Int),
                  "hasTakenValidRide" A..= False,
                  "isValidRide" A..= (Nothing :: Maybe Bool)
                ]
        rewardContextToLogicInput defaultRewardContext @?= expected,
      -- collectVarNames: extract every {"var": ...} the logic references
      testCase "collectVarNames: simple var reference" $
        Eval.collectVarNames (gteRule "ridesLast7d" 5) @?= ["ridesLast7d"],
      testCase "collectVarNames: nested operators, in traversal order" $
        Eval.collectVarNames (andRule [gteRule "ridesLast7d" 5, eqRule "hasTakenValidRide" True])
          @?= ["ridesLast7d", "hasTakenValidRide"],
      testCase "collectVarNames: array form {\"var\":[name, default]}" $
        Eval.collectVarNames (varWithDefault "ridesLast30d" 0) @?= ["ridesLast30d"],
      testCase "collectVarNames: whole-context {\"var\":\"\"} yields empty string" $
        Eval.collectVarNames (varRule "") @?= [""],
      -- rewardContextKeys is the single source of truth for allowed field names
      testCase "rewardContextKeys contains known fields but not typos" $ do
        ("ridesLast7d" `elem` rewardContextKeys) @?= True
        ("hasTakenValidRide" `elem` rewardContextKeys) @?= True
        ("isValidRide" `elem` rewardContextKeys) @?= True
        ("ridesLast7days" `elem` rewardContextKeys) @?= False,
      testCase "unknown-var detection flags a typo'd field (mirrors handler check)" $ do
        let logic = gteRule "ridesLast7days" 5
            referenced = filter (not . T.null) (Eval.collectVarNames logic)
            unknown = filter (\v -> T.takeWhile (/= '.') v `notElem` rewardContextKeys) referenced
        unknown @?= ["ridesLast7days"],
      -- nextUnlockDecision: repeatable-cohort unlock engine (Task 1)
      testCase "non-repeatable cohort: second matching evaluation is a no-op (today's behavior, unchanged)" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) Nothing
            existing = [mkUnlock "c1" DRU.Active 1]
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort existing candidate @?= Nothing,
      testCase "non-repeatable cohort: first evaluation with no existing rows unlocks at seq 1" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) Nothing
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort [] candidate @?= Just 1,
      testCase "repeatable cohort under cap: two evaluations create unlockSeq 1 then 2, both Active" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) (Just 3)
            candidate = mkCandidate "c1"
            -- First evaluation: no existing rows yet.
            firstSeq = QRUE.nextUnlockDecision cohort [] candidate
        firstSeq @?= Just 1
        let afterFirst = [mkUnlock "c1" DRU.Active 1]
            -- Second evaluation: the first unlock now exists (Active).
            secondSeq = QRUE.nextUnlockDecision cohort afterFirst candidate
        secondSeq @?= Just 2,
      testCase "repeatable cohort at cap: liveForCohort count reaching n makes the next evaluation a no-op" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) (Just 3)
            existing = [mkUnlock "c1" DRU.Active 1, mkUnlock "c1" DRU.Redeemed 2, mkUnlock "c1" DRU.Active 3]
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort existing candidate @?= Nothing,
      testCase "reclaimed row does not count against the cap and its unlockSeq is not reused" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) (Just 2)
            -- 2 live rows (seq 1, 3) plus a Reclaimed row at seq 2: cap is 2,
            -- liveForCohort has 2 entries, so this should still be blocked...
            existingAtCap = [mkUnlock "c1" DRU.Active 1, mkUnlock "c1" DRU.Reclaimed 2, mkUnlock "c1" DRU.Active 3]
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort existingAtCap candidate @?= Nothing
        -- ...but with only 1 live row (the Reclaimed one doesn't count against
        -- the cap), the rider is under cap. nextSeq is still computed from the
        -- max unlockSeq across ALL rows (including Reclaimed), so it continues
        -- from 2 rather than reusing it: 3.
        let existingUnderCap = [mkUnlock "c1" DRU.Active 1, mkUnlock "c1" DRU.Reclaimed 2]
        QRUE.nextUnlockDecision cohort existingUnderCap candidate @?= Just 3,
      -- unlockSeq is nullable (NammaDSL forbids a NOT NULL new column); rows
      -- written before this migration have unlockSeq = Nothing and must be
      -- treated as occupying seq 1, both for blocking non-repeatable re-unlocks
      -- and for next-seq computation on repeatable cohorts.
      testCase "legacy row (unlockSeq = Nothing) blocks a non-repeatable cohort's re-unlock" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) Nothing
            existing = [mkLegacyUnlock "c1" DRU.Active]
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort existing candidate @?= Nothing,
      testCase "legacy row (unlockSeq = Nothing) is treated as seq 1 when computing the next seq" $ do
        let cohort = mkCohortWithCap "c1" (gteRule "ridesLast7d" 5) (Just 3)
            existing = [mkLegacyUnlock "c1" DRU.Active]
            candidate = mkCandidate "c1"
        QRUE.nextUnlockDecision cohort existing candidate @?= Just 2
    ]

mkCtx :: Text -> Int -> A.Value
mkCtx field n = A.Object $ KM.fromList [(AK.fromText field, A.toJSON n)]

gteRule :: Text -> Int -> A.Value
gteRule field n =
  A.Object $
    KM.fromList
      [ (">=", A.Array $ V.fromList [A.Object $ KM.fromList [("var", A.String field)], A.toJSON n])
      ]

varRule :: Text -> A.Value
varRule name = A.object ["var" A..= name]

varWithDefault :: Text -> Int -> A.Value
varWithDefault name def = A.object ["var" A..= [A.String name, A.toJSON def]]

andRule :: [A.Value] -> A.Value
andRule rules = A.object ["and" A..= rules]

eqRule :: Text -> Bool -> A.Value
eqRule field b = A.object ["==" A..= [varRule field, A.toJSON b]]

mkCohort :: Text -> A.Value -> DRC.RewardCohort
mkCohort cohortId rule = mkCohortWithCap cohortId rule Nothing

mkCohortWithCap :: Text -> A.Value -> Maybe Int -> DRC.RewardCohort
mkCohortWithCap cohortId rule maxUnlocks =
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
      maxUnlocksPerCohort = maxUnlocks,
      presentation = Nothing,
      createdAt = read "2026-01-01 00:00:00 UTC",
      updatedAt = read "2026-01-01 00:00:00 UTC",
      merchantId = Nothing,
      merchantOperatingCityId = Nothing
    }

-- | A minimal existing 'RewardUnlock' row for a given cohort, status, and
-- unlockSeq — only the fields 'nextUnlockDecision' inspects vary per test.
mkUnlock :: Text -> DRU.UnlockStatus -> Int -> DRU.RewardUnlock
mkUnlock cohortId status seqNo = mkUnlockWithSeq cohortId status (Just seqNo)

-- | A row with unlockSeq = Nothing, as written before the unlockSeq column
-- existed (NammaDSL forbids a NOT NULL new column, so unlockSeq is nullable
-- and pre-migration rows are never backfilled).
mkLegacyUnlock :: Text -> DRU.UnlockStatus -> DRU.RewardUnlock
mkLegacyUnlock cohortId status = mkUnlockWithSeq cohortId status Nothing

mkUnlockWithSeq :: Text -> DRU.UnlockStatus -> Maybe Int -> DRU.RewardUnlock
mkUnlockWithSeq cohortId status seqNo =
  DRU.RewardUnlock
    { id = Id "unlock-1",
      personId = Id "test-rider",
      campaignId = Id "test-campaign",
      cohortId = Id cohortId,
      unlockedAt = read "2026-01-01 00:00:00 UTC",
      couponCode = Nothing,
      couponSource = DRCmp.Templated,
      couponValidTill = Nothing,
      status = status,
      unlockSeq = seqNo,
      viewedAt = Nothing,
      claimedAt = Nothing,
      redeemedAt = Nothing,
      reclaimedAt = Nothing,
      createdAt = read "2026-01-01 00:00:00 UTC",
      updatedAt = read "2026-01-01 00:00:00 UTC",
      merchantId = Nothing,
      merchantOperatingCityId = Nothing
    }

-- | The candidate row 'evaluateRewardsForRider' would build before calling
-- 'createNextUnlock'; only 'cohortId' matters for 'nextUnlockDecision'.
mkCandidate :: Text -> DRU.RewardUnlock
mkCandidate cohortId = mkUnlock cohortId DRU.Active 1
