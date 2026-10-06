{-# LANGUAGE OverloadedStrings #-}

module RewardsEvaluatorTests (tests) where

import qualified API.Types.UI.Rewards as API
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Time (addUTCTime)
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Action.Rewards.LiveReward as LiveReward
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
        QRUE.nextUnlockDecision cohort existing candidate @?= Just 2,
      -- Live reward card (GET /rewards/live)
      testCase "live reward: campaigns then cohorts by (displayOrder, createdAt), so ties are stable" $ do
        let campA = mkCampaign "campA" 0 (day 2)
            campB = mkCampaign "campB" 0 (day 1)
            candidates =
              LiveReward.mkLiveRewardCandidates
                [ (campA, [mkHomeCardCohort "a2" 1 (day 0) Nothing (Just (cardJson "A2")), mkHomeCardCohort "a1" 0 (day 0) Nothing (Just (cardJson "A1"))]),
                  (campB, [mkHomeCardCohort "b1" 0 (day 0) Nothing (Just (cardJson "B1"))])
                ]
        map candidateCohortId candidates @?= ["b1", "a1", "a2"]
        map candidateTitle candidates @?= ["B1", "A1", "A2"],
      testCase "live reward: cohorts without a valid homeCard and non-Active campaigns are skipped" $ do
        let active = mkCampaign "active" 0 (day 0)
            paused = mkCampaignWith "paused" 0 (day 0) DRCmp.Paused Nothing
            candidates =
              LiveReward.mkLiveRewardCandidates
                [ ( active,
                    [ mkHomeCardCohort "noPresentation" 0 (day 0) Nothing Nothing,
                      mkHomeCardCohort "noTitle" 1 (day 0) Nothing (Just (A.object ["header" A..= ("hi" :: Text)])),
                      mkHomeCardCohort "ok" 2 (day 0) Nothing (Just (cardJson "OK"))
                    ]
                  ),
                  (paused, [mkHomeCardCohort "pausedCohort" 0 (day 0) Nothing (Just (cardJson "P"))])
                ]
        map candidateCohortId candidates @?= ["ok"],
      testCase "live reward: audience rule may only use home-screen context fields" $ do
        let parsed logic = LiveReward.parseHomeCard (cardWithAudience "T" logic)
        isRight' (parsed (eqRule "hasTakenValidRide" False)) @?= True
        isRight' (parsed (eqRule "isValidRide" True)) @?= False
        isRight' (parsed (gteRule "ridesLast7days" 3)) @?= False
        isRight' (LiveReward.parseHomeCard (A.object ["header" A..= ("no title" :: Text)])) @?= False,
      testCase "live reward: live only inside the campaign window" $ do
        let campaign = mkCampaignWith "c" 0 (day 0) DRCmp.Active (Just (day 10))
            candidates = LiveReward.mkLiveRewardCandidates [(campaign, [mkHomeCardCohort "c1" 0 (day 0) Nothing (Just (cardJson "C1"))])]
            liveAt d = map (LiveReward.isLiveAt (day d)) candidates
        liveAt 5 @?= [True]
        liveAt (-1) @?= [False]
        liveAt 10 @?= [False],
      testCase "live reward: one-shot cohort hides once unlocked in any non-Reclaimed status" $ do
        let candidates = singleCohortCandidates Nothing Nothing
            shown unlocks = map candidateCohortId (LiveReward.eligibleLiveRewards A.Null unlocks candidates)
        shown [] @?= ["c1"]
        shown [mkUnlock "c1" DRU.Active 1] @?= []
        shown [mkUnlock "c1" DRU.Redeemed 1] @?= []
        shown [mkUnlock "c1" DRU.ExpiredUnredeemed 1] @?= []
        shown [mkUnlock "other" DRU.Active 1] @?= ["c1"],
      testCase "live reward: a Reclaimed unlock shows the card again" $ do
        let candidates = singleCohortCandidates Nothing Nothing
        map candidateCohortId (LiveReward.eligibleLiveRewards A.Null [mkUnlock "c1" DRU.Reclaimed 1] candidates) @?= ["c1"],
      testCase "live reward: repeatable cohort keeps its card until the cap is reached" $ do
        let candidates = singleCohortCandidates (Just 2) Nothing
            shown unlocks = map candidateCohortId (LiveReward.eligibleLiveRewards A.Null unlocks candidates)
        shown [mkUnlock "c1" DRU.Active 1] @?= ["c1"]
        shown [mkUnlock "c1" DRU.Active 1, mkUnlock "c1" DRU.Redeemed 2] @?= []
        shown [mkUnlock "c1" DRU.Active 1, mkUnlock "c1" DRU.Reclaimed 2] @?= ["c1"],
      testCase "live reward: audience rule is checked against the rider's current context" $ do
        let candidates = singleCohortCandidates Nothing (Just (eqRule "hasTakenValidRide" False))
            shownFor hasRide =
              map candidateCohortId $
                LiveReward.eligibleLiveRewards
                  (rewardContextToLogicInput defaultRewardContext {hasTakenValidRide = Just hasRide})
                  []
                  candidates
        shownFor False @?= ["c1"]
        shownFor True @?= [],
      testCase "live reward: falls through to the next candidate when the first is unlocked" $ do
        let campaign = mkCampaign "camp" 0 (day 0)
            candidates =
              LiveReward.mkLiveRewardCandidates
                [(campaign, [mkHomeCardCohort "c1" 0 (day 0) Nothing (Just (cardJson "C1")), mkHomeCardCohort "c2" 1 (day 0) Nothing (Just (cardJson "C2"))])]
        map candidateTitle (LiveReward.eligibleLiveRewards A.Null [mkUnlock "c1" DRU.Active 1] candidates) @?= ["C2"]
    ]

mkCtx :: Text -> Int -> A.Value
mkCtx field n = A.Object $ KM.fromList [(field, A.toJSON n)]

gteRule :: Text -> Int -> A.Value
gteRule field n =
  A.Object $
    KM.fromList
      [ (">=", A.Array $ fromList [A.Object $ KM.fromList [("var", A.String field)], A.toJSON n])
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

-- Live reward card helpers

day :: Integer -> UTCTime
day n = addUTCTime (fromInteger (n * 86400)) (read "2026-01-01 00:00:00 UTC")

cardJson :: Text -> A.Value
cardJson title = A.object ["title" A..= title]

cardWithAudience :: Text -> A.Value -> A.Value
cardWithAudience title logic = A.object ["title" A..= title, "targetingJsonLogic" A..= logic]

isRight' :: Either a b -> Bool
isRight' = either (const False) (const True)

candidateCohortId :: LiveReward.LiveRewardCandidate -> Text
candidateCohortId LiveReward.LiveRewardCandidate {cohortId = cid} = getId cid

candidateTitle :: LiveReward.LiveRewardCandidate -> Text
candidateTitle LiveReward.LiveRewardCandidate {card = API.LiveRewardCard {title = cardTitle}} = cardTitle

-- | One Active campaign with a single home-card cohort "c1".
singleCohortCandidates :: Maybe Int -> Maybe A.Value -> [LiveReward.LiveRewardCandidate]
singleCohortCandidates maxUnlocks audience =
  LiveReward.mkLiveRewardCandidates
    [(mkCampaign "camp" 0 (day 0), [mkHomeCardCohort "c1" 0 (day 0) maxUnlocks (Just (maybe (cardJson "C1") (cardWithAudience "C1") audience))])]

mkHomeCardCohort :: Text -> Int -> UTCTime -> Maybe Int -> Maybe A.Value -> DRC.RewardCohort
mkHomeCardCohort cohortIdText order created maxUnlocks homeCard =
  DRC.RewardCohort
    { id = Id cohortIdText,
      campaignId = Id "test-campaign",
      name = "test",
      description = Nothing,
      displayOrder = order,
      eligibilityJsonLogic = A.Bool True,
      rewardTitle = "test",
      rewardImageUrl = Nothing,
      couponValidityDays = Nothing,
      maxUnlocksPerCohort = maxUnlocks,
      presentation = (\h -> A.object ["homeCard" A..= h]) <$> homeCard,
      createdAt = created,
      updatedAt = created,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing
    }

mkCampaign :: Text -> Int -> UTCTime -> DRCmp.RewardCampaign
mkCampaign campaignIdText order created = mkCampaignWith campaignIdText order created DRCmp.Active Nothing

mkCampaignWith :: Text -> Int -> UTCTime -> DRCmp.CampaignStatus -> Maybe UTCTime -> DRCmp.RewardCampaign
mkCampaignWith campaignIdText order created campaignStatus campaignEndsAt =
  DRCmp.RewardCampaign
    { id = Id campaignIdText,
      merchantId = Id "test-merchant",
      merchantOperatingCityId = Id "test-city",
      name = "test",
      description = Nothing,
      sponsorType = DRCmp.Internal,
      sponsorName = "test",
      sponsorLogoUrl = Nothing,
      couponSourceType = DRCmp.Templated,
      couponTemplate = Nothing,
      redemptionTargetType = DRCmp.InApp,
      redemptionTargetUrl = Nothing,
      claimMode = DRCmp.AutoClaim,
      reclaimPolicy = Nothing,
      startsAt = day 0,
      endsAt = campaignEndsAt,
      status = campaignStatus,
      displayOrder = order,
      createdBy = "test",
      createdAt = created,
      updatedAt = created
    }
