module DynamicLogicSpec (dynamicLogicTests) where

import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogicRollout (AppDynamicLogicRollout (..))
import Test.Tasty
import Test.Tasty.HUnit
import Tools.DynamicLogic (cumulativeRollout, filterActiveRollouts, findLogic, ungroupBaseRollouts)

-- | A fixed timestamp so rollout fixtures don't need the clock.
defaultTime :: UTCTime
defaultTime = Time.UTCTime (Time.fromGregorian 2025 1 1) (Time.secondsToDiffTime 0)

-- | Base fixture; individual tests override only the fields they care about.
sampleRollout :: AppDynamicLogicRollout
sampleRollout =
  AppDynamicLogicRollout
    { domain = LYT.POOLING,
      experimentGroup = Nothing,
      experimentStatus = Just LYT.RUNNING,
      isBaseVersion = Nothing,
      merchantId = Nothing,
      merchantOperatingCityId = Id "test-moc",
      modifiedBy = Nothing,
      percentageRollout = 0,
      timeBounds = "Unbounded",
      version = 0,
      versionDescription = Nothing,
      createdAt = defaultTime,
      updatedAt = defaultTime
    }

mkR :: Int -> Int -> AppDynamicLogicRollout
mkR ver pct = sampleRollout {version = ver, percentageRollout = pct}

-- | Deterministic twin of 'Tools.DynamicLogic.chooseLogic' for a fixed toss:
-- chooseLogic logics (Just toss) == pure (findLogic toss (cumulativeRollout logics)).
selectVer :: Int -> [AppDynamicLogicRollout] -> Maybe Int
selectVer toss rollouts = (.version) <$> findLogic toss (cumulativeRollout rollouts)

dynamicLogicTests :: TestTree
dynamicLogicTests =
  testGroup
    "Dynamic logic rollout selection"
    [ cumulativeRolloutTests,
      findLogicBoundaryTests,
      selectionDistributionTests,
      underHundredTests,
      filterActiveRolloutsTests,
      ungroupBaseRolloutsTests
    ]

-- cumulativeRollout should produce running prefix sums of percentageRollout.
cumulativeRolloutTests :: TestTree
cumulativeRolloutTests =
  testGroup
    "cumulativeRollout (prefix sums)"
    [ testCase "empty" $
        map snd (cumulativeRollout []) @?= [],
      testCase "single version" $
        map snd (cumulativeRollout [mkR 1 40]) @?= [40],
      testCase "three versions summing to 100" $
        map snd (cumulativeRollout [mkR 1 30, mkR 2 50, mkR 3 20]) @?= [30, 80, 100],
      testCase "preserves order and pairs each cumulative with its version" $
        map (\(r, c) -> (r.version, c)) (cumulativeRollout [mkR 7 10, mkR 9 25])
          @?= [(7, 10), (9, 35)]
    ]

-- findLogic uses randVal <= cumPercent, so boundaries are inclusive on the lower
-- version. These cases pin the off-by-one behaviour.
findLogicBoundaryTests :: TestTree
findLogicBoundaryTests =
  testGroup
    "findLogic (bucket boundaries, 30/50/20)"
    [ testCase "toss 1 -> v1" $ selectVer 1 rs @?= Just 1,
      testCase "toss 30 (upper edge of v1) -> v1" $ selectVer 30 rs @?= Just 1,
      testCase "toss 31 (first of v2) -> v2" $ selectVer 31 rs @?= Just 2,
      testCase "toss 80 (upper edge of v2) -> v2" $ selectVer 80 rs @?= Just 2,
      testCase "toss 81 (first of v3) -> v3" $ selectVer 81 rs @?= Just 3,
      testCase "toss 100 (upper edge of v3) -> v3" $ selectVer 100 rs @?= Just 3,
      testCase "empty rollout list -> Nothing" $ selectVer 50 [] @?= Nothing,
      testCase "first matching version wins" $
        selectVer 5 [mkR 1 10, mkR 2 10] @?= Just 1
    ]
  where
    rs = [mkR 1 30, mkR 2 50, mkR 3 20]

-- Sweeping the whole 1..100 toss space, each version must receive exactly its
-- configured share and every toss must land on some version.
selectionDistributionTests :: TestTree
selectionDistributionTests =
  testGroup
    "end-to-end selection distribution (30/50/20)"
    [ testCase "v1 gets 30 tosses" $ count (Just 1) @?= 30,
      testCase "v2 gets 50 tosses" $ count (Just 2) @?= 50,
      testCase "v3 gets 20 tosses" $ count (Just 3) @?= 20,
      testCase "no toss in 1..100 is unassigned" $ count Nothing @?= 0
    ]
  where
    rs = [mkR 1 30, mkR 2 50, mkR 3 20]
    picks = [selectVer t rs | t <- [1 .. 100]]
    count v = length (filter (== v) picks)

-- When versions sum to < 100, tosses above the total intentionally match no
-- experiment version (the caller then falls back to the base version).
underHundredTests :: TestTree
underHundredTests =
  testGroup
    "partial coverage (30/40 = 70%)"
    [ testCase "toss 70 (last covered) -> v2" $ selectVer 70 rs @?= Just 2,
      testCase "toss 71 (above total) -> Nothing" $ selectVer 71 rs @?= Nothing,
      testCase "toss 100 -> Nothing" $ selectVer 100 rs @?= Nothing
    ]
  where
    rs = [mkR 1 30, mkR 2 40]

-- filterActiveRollouts drops DISCARDED and REVERTED, keeps the rest.
filterActiveRolloutsTests :: TestTree
filterActiveRolloutsTests =
  testGroup
    "filterActiveRollouts"
    [ testCase "keeps RUNNING / CONCLUDED / no-status, drops DISCARDED / REVERTED" $
        map (.version) (filterActiveRollouts rollouts) @?= [1, 4, 5]
    ]
  where
    withStatus ver st = (mkR ver 10) {experimentStatus = st}
    rollouts =
      [ withStatus 1 (Just LYT.RUNNING),
        withStatus 2 (Just LYT.DISCARDED),
        withStatus 3 (Just LYT.REVERTED),
        withStatus 4 (Just LYT.CONCLUDED),
        withStatus 5 Nothing
      ]

-- ungroupBaseRollouts strips the experiment group from base versions only.
ungroupBaseRolloutsTests :: TestTree
ungroupBaseRolloutsTests =
  testGroup
    "ungroupBaseRollouts"
    [ testCase "base version has its experimentGroup cleared" $
        experimentGroup (head (ungroupBaseRollouts [baseGrouped])) @?= Nothing,
      testCase "non-base version keeps its experimentGroup" $
        experimentGroup (head (ungroupBaseRollouts [experimentGrouped])) @?= Just "grpA"
    ]
  where
    baseGrouped = (mkR 1 100) {isBaseVersion = Just True, experimentGroup = Just "grpA"}
    experimentGrouped = (mkR 2 50) {isBaseVersion = Just False, experimentGroup = Just "grpA"}
