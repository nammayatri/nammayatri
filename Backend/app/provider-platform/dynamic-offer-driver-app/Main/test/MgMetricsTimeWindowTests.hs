{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | includeMetricsAllDay: with a milestone time window and the flag false, behaviour metrics, rule-visible
-- ride counters, MG online slabs and payout % rules count only the *InsideTimebound values.
module MgMetricsTimeWindowTests (tests) where

import Data.Aeson (Value (..), fromJSON, object, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HM
import Data.Time (TimeOfDay (..), fromGregorian)
import qualified "mobility-core" Kernel.Types.TimeBound as TB
import qualified "incentive-journey" Lib.IncentiveJourney.BehaviorMetric as BM
import qualified "incentive-journey" Lib.IncentiveJourney.Deferred as Deferred
import qualified "incentive-journey" Lib.IncentiveJourney.Evaluation as Evaluation
import qualified "incentive-journey" Lib.IncentiveJourney.RewardMetadata as RM
import "incentive-journey" Lib.IncentiveJourney.Types (ConditionOperator (GTE), ConditionType (..))
import qualified "incentive-journey" Lib.IncentiveJourney.Types as IJT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

window :: Maybe TB.TimeBound
window = Just (TB.BoundedByDay [(fromGregorian 2026 11 24, [(TimeOfDay 8 0 0, TimeOfDay 10 0 0)])])

insideOnly :: Evaluation.MetricScope
insideOnly = Evaluation.InsideTimeboundMetrics

allDay :: Evaluation.MetricScope
allDay = Evaluation.AllDayMetrics

-- | Stats currentMetrics: 30 online minutes / 8 of 10 requests accepted inside the window,
-- 60 minutes / 0 of 10 outside; 5 rides all day (flat, from the ride bag), 2 of them inside.
statsMetrics :: Maybe Value
statsMetrics =
  Evaluation.enrichCurrentMetricsWithDerivedRates . Just $
    object
      [ "onlineMinutesInsideTimebound" .= (30 :: Int),
        "onlineMinutesOutsideTimebound" .= (60 :: Int),
        "rrReceivedInsideTimebound" .= (10 :: Int),
        "rrReceivedOutsideTimebound" .= (10 :: Int),
        "rrAcceptedInsideTimebound" .= (8 :: Int),
        "rrAcceptedOutsideTimebound" .= (0 :: Int),
        "ridesCompleted" .= (5 :: Int),
        "ridesCompletedInsideTimebound" .= (2 :: Int),
        "earnings" .= (900 :: Int),
        "earningsInsideTimebound" .= (300 :: Int)
      ]

numberAt :: String -> Maybe Value -> Maybe Int
numberAt key mb = case mb of
  Just (Object o) -> case AKM.lookup (AKey.fromString key) o of
    Just (Number n) -> Just (round n)
    _ -> Nothing
  _ -> Nothing

mgMeta :: Maybe Bool -> Maybe Value
mgMeta mbFlag =
  Just . object $
    [ "mgAmountSlabs" .= [object ["fromValue" .= (0 :: Int), "amount" .= (0 :: Int)], object ["fromValue" .= (60 :: Int), "amount" .= (200 :: Int)]]
    ]
      <> maybe [] (\f -> ["includeMetricsAllDay" .= f]) mbFlag

percentMeta :: Bool -> RM.MilestoneRewardMeta
percentMeta flag =
  RM.MilestoneRewardMeta
    { RM.includeEarningsAllDay = False,
      RM.includeMetricsAllDay = flag,
      RM.mgAmountSlabs = [],
      RM.mgAmountSlabsFromValueUnit = RM.Minutes,
      RM.percentSlabs = [RM.MetricPercentRule "acceptanceRate" [RM.PercentSlab 0 0, RM.PercentSlab 80 100]],
      RM.violationPercentSlabs = [],
      RM.violationMetrics = []
    }

tests :: TestTree
tests =
  testGroup
    "MG metrics time window (includeMetricsAllDay)"
    [ testGroup
        "flag parsing"
        [ testCase "metricScopeFor: false + window -> inside only" $
            Evaluation.metricScopeFor False window @?= insideOnly,
          testCase "metricScopeFor: true + window -> all day" $
            Evaluation.metricScopeFor True window @?= allDay,
          testCase "metricScopeFor: false without a window (Nothing / Unbounded) -> all day" $
            (Evaluation.metricScopeFor False Nothing, Evaluation.metricScopeFor False (Just TB.Unbounded)) @?= (allDay, allDay),
          testCase "evaluation flag: absent / no evaluation -> true, false -> false" $
            ( Evaluation.evaluationIncludeMetricsAllDay Nothing,
              Evaluation.evaluationIncludeMetricsAllDay (Just (object ["logic" .= object []])),
              Evaluation.evaluationIncludeMetricsAllDay (Just (object ["includeMetricsAllDay" .= False])),
              Evaluation.evaluationIncludeMetricsAllDay (Just (object ["logic" .= object [], "includeMetricsAllDay" .= True]))
            )
              @?= (True, True, False, True),
          testCase "rewardMetadata: absent -> true; false parsed; encoded" $ do
            (RM.parseMilestoneRewardMeta (mgMeta Nothing)).includeMetricsAllDay @?= True
            (RM.parseMilestoneRewardMeta (mgMeta (Just False))).includeMetricsAllDay @?= False
            (RM.parseMilestoneRewardMeta Nothing).includeMetricsAllDay @?= True
            numberOrBool "includeMetricsAllDay" (toJSON (RM.parseMilestoneRewardMeta (mgMeta (Just False)))) @?= Just (Bool False),
          testCase "streak meta: absent -> true; false parsed, encoded and carried by streakMetaAsMilestone" $ do
            let sm = RM.parseStreakRewardMeta (Just (object ["includeMetricsAllDay" .= False]))
            (RM.parseStreakRewardMeta (Just (object []))).includeMetricsAllDay @?= True
            sm.includeMetricsAllDay @?= False
            (Deferred.streakMetaAsMilestone sm).includeMetricsAllDay @?= False
            fmap (.includeMetricsAllDay) (fromJSON (toJSON sm) :: A.Result RM.StreakRewardMeta) @?= A.Success False
        ],
      testGroup
        "evaluationLogic"
        [ testCase "flag-only object is not a rule" $
            Evaluation.evaluationLogic (Just (object ["includeMetricsAllDay" .= False])) @?= Nothing,
          testCase "logic + flag -> the logic" $
            Evaluation.evaluationLogic (Just (object ["logic" .= rule60, "includeMetricsAllDay" .= False])) @?= Just rule60,
          testCase "stored bare rule (no wrapper, no flag) -> the rule" $
            Evaluation.evaluationLogic (Just rule60) @?= Just rule60,
          testCase "logic null / no evaluation -> no rule" $
            (Evaluation.evaluationLogic (Just (object ["logic" .= Null])), Evaluation.evaluationLogic Nothing) @?= (Nothing, Nothing)
        ],
      testGroup
        "lookups"
        [ testCase "all day sums inside + outside; inside only reads the inside key" $
            ( Evaluation.lookupMetricIntIn allDay statsMetrics mempty "onlineMinutes",
              Evaluation.lookupMetricIntIn insideOnly statsMetrics mempty "onlineMinutes"
            )
              @?= (90, 30),
          testCase "inside only: missing inside key -> 0 (never the flat all-day key)" $
            Evaluation.lookupMetricIntIn insideOnly (Just (object ["dxCancel" .= (3 :: Int)])) mempty "dxCancel" @?= 0,
          testCase "rates: all day 40% (8/20), inside only 80% (8/10)" $
            ( Evaluation.lookupMetricIntIn allDay statsMetrics mempty "acceptanceRate",
              Evaluation.lookupMetricIntIn insideOnly statsMetrics mempty "acceptanceRate"
            )
              @?= (40, 80),
          testCase "ride counters: all day keeps the flat value (5), inside only 2" $
            ( Evaluation.lookupMetricIntIn allDay statsMetrics mempty "ridesCompleted",
              Evaluation.lookupMetricIntIn insideOnly statsMetrics mempty "ridesCompleted"
            )
              @?= (5, 2),
          testCase "MG online minutes in both scopes" $
            (Evaluation.lookupOnlineMinutes allDay statsMetrics mempty, Evaluation.lookupOnlineMinutes insideOnly statsMetrics mempty) @?= (90, 30),
          testCase "metric context: inside only has inside counters + derived rates" $ do
            let BM.MetricContext {vars = v} = BM.metricContextFromCurrentMetrics insideOnly statsMetrics
            map (\k -> HM.lookupDefault (-1) k v) ["onlineMinutes", "rrReceived", "acceptanceRate", "ridesCompleted", "earnings"] @?= [30, 10, 80, 2, 300],
          testCase "metric context: all day unchanged (totals)" $ do
            let BM.MetricContext {vars = v} = BM.metricContextFromCurrentMetrics allDay statsMetrics
            map (\k -> HM.lookupDefault (-1) k v) ["onlineMinutes", "rrReceived", "acceptanceRate", "ridesCompleted", "earnings"] @?= [90, 20, 40, 5, 900],
          testCase "scoped context: inside only ignores the all-day ride bag, keeps ij:bm behaviour counts" $ do
            let bag = BM.MetricContext {vars = HM.fromList [("ridesCompleted", 7), ("onlineMinutes", 45)]}
                BM.MetricContext {vars = vIn} = BM.scopedMetricContext insideOnly statsMetrics bag
                BM.MetricContext {vars = vAll} = BM.scopedMetricContext allDay statsMetrics bag
            (HM.lookup "ridesCompleted" vIn, HM.lookup "onlineMinutes" vIn) @?= (Just 2, Just 45)
            (HM.lookup "ridesCompleted" vAll, HM.lookup "onlineMinutes" vAll) @?= (Just 7, Just 90)
        ],
      testGroup
        "conditions"
        [ testCase "simple OnlineMinutes >= 60 with a flag-only evaluation: inside 30 fails, all day 90 passes" $ do
            let ev = Just (object ["includeMetricsAllDay" .= False])
                cond scope = BM.evaluateBehaviorCondition (Evaluation.evaluationLogic ev) OnlineMinutes GTE 60 (BM.metricContextFromCurrentMetrics scope statsMetrics)
            (cond insideOnly, cond allDay) @?= (Right False, Right True),
          testCase "simple AcceptanceRate >= 60: inside 80 passes, all day 40 fails" $ do
            let cond scope = BM.evaluateBehaviorCondition Nothing AcceptanceRate GTE 60 (BM.metricContextFromCurrentMetrics scope statsMetrics)
            (cond insideOnly, cond allDay) @?= (Right True, Right False),
          testCase "JSONLogic onlineMinutes >= 60 with logic + flag: inside fails, all day passes" $ do
            let ev = Just (object ["logic" .= rule60, "includeMetricsAllDay" .= False])
                cond scope = BM.evaluateBehaviorCondition (Evaluation.evaluationLogic ev) JSONLogic IJT.EQ 0 (BM.metricContextFromCurrentMetrics scope statsMetrics)
            (cond insideOnly, cond allDay) @?= (Right False, Right True)
        ],
      testGroup
        "merge"
        [ testCase "scoped merge never copies outside-window counts into InsideTimebound" $ do
            let merged = Evaluation.mergeCurrentMetricsScoped statsMetrics (HM.fromList [("onlineMinutes", 90)]) (HM.fromList [("onlineMinutes", 30)])
            (numberAt "onlineMinutesInsideTimebound" merged, numberAt "onlineMinutes" merged) @?= (Just 30, Just 90)
            Evaluation.lookupMetricIntIn allDay merged mempty "onlineMinutes" @?= 90,
          testCase "ride-end inside bump adds to the inside ride keys only" $ do
            let bumped = Evaluation.bumpInsideRideCountersInCurrentMetrics statsMetrics [("ridesCompleted", 1), ("distanceMeters", 0)]
            (numberAt "ridesCompletedInsideTimebound" bumped, numberAt "ridesCompleted" bumped, numberAt "distanceMetersInsideTimebound" bumped) @?= (Just 3, Just 5, Nothing)
        ],
      testGroup
        "MG payout"
        [ testCase "MG online slab: flag false + window -> 30 inside minutes -> 0" $
            Deferred.computeNetOffMilestonePayout window (mgMeta (Just False)) Nothing statsMetrics @?= 0,
          testCase "MG online slab: flag true / absent -> 90 minutes -> 200" $
            ( Deferred.computeNetOffMilestonePayout window (mgMeta (Just True)) Nothing statsMetrics,
              Deferred.computeNetOffMilestonePayout window (mgMeta Nothing) Nothing statsMetrics
            )
              @?= (200, 200),
          testCase "MG online slab: flag false without a window -> no effect (200)" $
            Deferred.computeNetOffMilestonePayout Nothing (mgMeta (Just False)) Nothing statsMetrics @?= 200,
          testCase "percent rule on acceptanceRate: inside 80 -> 100%, all day 40 -> 0%" $
            ( Deferred.resolvePayoutPercent (Deferred.payoutMetricScope (percentMeta False) window) (percentMeta False) statsMetrics,
              Deferred.resolvePayoutPercent (Deferred.payoutMetricScope (percentMeta True) window) (percentMeta True) statsMetrics
            )
              @?= (100, 0),
          testCase "streak-end MG: flag false + campaign window -> inside minutes only" $ do
            let sm flag = RM.parseStreakRewardMeta (mgMeta flag)
                rows = [("2026-11-24", statsMetrics)]
            ( Deferred.computeNetOffStreakPayout (sm (Just False)) window Nothing rows,
              Deferred.computeNetOffStreakPayout (sm Nothing) window Nothing rows,
              Deferred.computeNetOffStreakPayout (sm (Just False)) Nothing Nothing rows
              )
              @?= (0, 200, 200)
        ]
    ]
  where
    rule60 = object [">=" .= [object ["var" .= ("onlineMinutes" :: String)], Number 60]]
    numberOrBool key v = case v of
      Object o -> AKM.lookup (AKey.fromString key) o
      _ -> Nothing
