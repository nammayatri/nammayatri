{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | rewardMetadata / streakEndRewardMetadata parsers for NetOff + streak periodDays.
-- Slabs live here; evaluation column is condition JsonLogic only.
module Lib.IncentiveJourney.RewardMetadata
  ( AmountSlab (..),
    PercentSlab (..),
    MetricPercentRule (..),
    MilestoneRewardMeta (..),
    StreakRewardMeta (..),
    parseMilestoneRewardMeta,
    parseStreakRewardMeta,
    injectPeriodDaysIntoStreakMeta,
    streakMetaPeriodDays,
    resolveAmountSlab,
    resolvePercentSlab,
    resolveMinPercent,
    -- | TODOCAMPAIGN: replace with real violation metric names when product finalizes.
    violationMetricNamesPlaceholder,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value, withObject, (.:), (.:?))
import qualified Data.Aeson as A
import Data.Ord (comparing)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Evaluation as Evaluation

-- | MG amount by metric threshold (currently online hours inside timebound).
data AmountSlab = AmountSlab
  { fromValue :: Int,
    amount :: Int
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON AmountSlab where
  parseJSON = withObject "AmountSlab" $ \o ->
    AmountSlab
      <$> o .: "fromValue"
      <*> o .: "amount"

instance A.ToJSON AmountSlab where
  toJSON AmountSlab {..} =
    A.object ["fromValue" A..= fromValue, "amount" A..= amount]

data PercentSlab = PercentSlab
  { fromValue :: Int,
    percent :: Int
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON PercentSlab where
  parseJSON = withObject "PercentSlab" $ \o ->
    PercentSlab
      <$> o .: "fromValue"
      <*> o .: "percent"

instance A.ToJSON PercentSlab where
  toJSON PercentSlab {..} =
    A.object ["fromValue" A..= fromValue, "percent" A..= percent]

data MetricPercentRule = MetricPercentRule
  { metric :: Text,
    slabs :: [PercentSlab]
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON MetricPercentRule where
  parseJSON = withObject "MetricPercentRule" $ \o ->
    MetricPercentRule
      <$> o .: "metric"
      <*> o .: "slabs"

instance A.ToJSON MetricPercentRule where
  toJSON MetricPercentRule {..} =
    A.object ["metric" A..= metric, "slabs" A..= slabs]

data MilestoneRewardMeta = MilestoneRewardMeta
  { includeEarningsAllDay :: Bool,
    -- | Online-hours → MG amount. Preferred over static rewardValue when non-empty.
    mgAmountSlabs :: [AmountSlab],
    -- | Per-metric % slabs; final payout uses minimum applicable %.
    percentSlabs :: [MetricPercentRule],
    -- | Violation-count % (sum of named inside-timebound metrics).
    violationPercentSlabs :: [PercentSlab],
    -- | Metric keys summed for violation %. Empty → use placeholder list.
    violationMetrics :: [Text]
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON MilestoneRewardMeta where
  parseJSON = withObject "MilestoneRewardMeta" $ \o ->
    MilestoneRewardMeta
      <$> (o .:? "includeEarningsAllDay" A..!= False)
      <*> (o .:? "mgAmountSlabs" A..!= [])
      <*> (o .:? "percentSlabs" A..!= [])
      <*> (o .:? "violationPercentSlabs" A..!= [])
      <*> (o .:? "violationMetrics" A..!= [])

instance A.ToJSON MilestoneRewardMeta where
  toJSON MilestoneRewardMeta {..} =
    A.object
      [ "includeEarningsAllDay" A..= includeEarningsAllDay,
        "mgAmountSlabs" A..= mgAmountSlabs,
        "percentSlabs" A..= percentSlabs,
        "violationPercentSlabs" A..= violationPercentSlabs,
        "violationMetrics" A..= violationMetrics
      ]

data StreakRewardMeta = StreakRewardMeta
  { includeEarningsAllDay :: Bool,
    mgAmountSlabs :: [AmountSlab],
    percentSlabs :: [MetricPercentRule],
    violationPercentSlabs :: [PercentSlab],
    violationMetrics :: [Text],
    -- | Days per streak settlement slice; must divide campaign length.
    periodDays :: Maybe Int,
    -- | SWO fields may share this JSON; ignored for NetOff.
    serviceName :: Maybe Text,
    waiveOffMode :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON StreakRewardMeta where
  parseJSON = withObject "StreakRewardMeta" $ \o ->
    StreakRewardMeta
      <$> (o .:? "includeEarningsAllDay" A..!= False)
      <*> (o .:? "mgAmountSlabs" A..!= [])
      <*> (o .:? "percentSlabs" A..!= [])
      <*> (o .:? "violationPercentSlabs" A..!= [])
      <*> (o .:? "violationMetrics" A..!= [])
      <*> (o .:? "periodDays")
      <*> (o .:? "serviceName")
      <*> (o .:? "waiveOffMode")

instance A.ToJSON StreakRewardMeta where
  toJSON StreakRewardMeta {..} =
    A.object $
      catMaybes
        [ Just ("includeEarningsAllDay" A..= includeEarningsAllDay),
          Just ("mgAmountSlabs" A..= mgAmountSlabs),
          Just ("percentSlabs" A..= percentSlabs),
          Just ("violationPercentSlabs" A..= violationPercentSlabs),
          Just ("violationMetrics" A..= violationMetrics),
          ("periodDays" A..=) <$> periodDays,
          ("serviceName" A..=) <$> serviceName,
          ("waiveOffMode" A..=) <$> waiveOffMode
        ]

parseMilestoneRewardMeta :: Maybe Value -> MilestoneRewardMeta
parseMilestoneRewardMeta = \case
  Nothing -> emptyMilestoneMeta
  Just v ->
    case A.fromJSON v of
      A.Success m -> m
      A.Error _ -> emptyMilestoneMeta

emptyMilestoneMeta :: MilestoneRewardMeta
emptyMilestoneMeta =
  MilestoneRewardMeta
    { includeEarningsAllDay = False,
      mgAmountSlabs = [],
      percentSlabs = [],
      violationPercentSlabs = [],
      violationMetrics = []
    }

parseStreakRewardMeta :: Maybe Value -> StreakRewardMeta
parseStreakRewardMeta = \case
  Nothing -> emptyStreakMeta
  Just v ->
    case A.fromJSON v of
      A.Success m -> m
      A.Error _ -> emptyStreakMeta

-- | Merge API periodDays into streakEndRewardMetadata JSON.
injectPeriodDaysIntoStreakMeta :: Maybe Int -> Maybe Value -> Maybe Value
injectPeriodDaysIntoStreakMeta mbPeriodDays mbMeta =
  case mbPeriodDays of
    Nothing | isNothing mbMeta -> mbMeta
    _ ->
      let meta = parseStreakRewardMeta mbMeta
          meta' = meta {periodDays = mbPeriodDays <|> meta.periodDays}
       in Just (A.toJSON meta')

streakMetaPeriodDays :: Maybe Value -> Maybe Int
streakMetaPeriodDays = (.periodDays) . parseStreakRewardMeta

emptyStreakMeta :: StreakRewardMeta
emptyStreakMeta =
  StreakRewardMeta
    { includeEarningsAllDay = False,
      mgAmountSlabs = [],
      percentSlabs = [],
      violationPercentSlabs = [],
      violationMetrics = [],
      periodDays = Nothing,
      serviceName = Nothing,
      waiveOffMode = Nothing
    }

-- | Highest fromValue <= metricValue wins; empty → 0.
-- Pick by threshold (not max amount/%) so decreasing violation/percent slabs work.
resolveAmountSlab :: [AmountSlab] -> Int -> Int
resolveAmountSlab slabs metricValue =
  let eligible = filter (\s -> s.fromValue <= metricValue) slabs
   in case eligible of
        [] -> 0
        xs -> (.amount) (maximumBy (comparing (.fromValue)) xs)

resolvePercentSlab :: [PercentSlab] -> Int -> Int
resolvePercentSlab slabs metricValue =
  let eligible = filter (\s -> s.fromValue <= metricValue) slabs
   in case eligible of
        [] -> 0
        xs -> (.percent) (maximumBy (comparing (.fromValue)) xs)

-- | Minimum % across metric percent rules + violation rule. Empty rules → Nothing.
resolveMinPercent ::
  Maybe Value -> -- currentMetrics
  [MetricPercentRule] ->
  [PercentSlab] -> -- violation
  [Text] -> -- violation metric names
  Maybe Int
resolveMinPercent mbMetrics percentRules violationSlabs violationNames =
  let metricPercents =
        map
          ( \rule ->
              let val = Evaluation.lookupMetricInt mbMetrics mempty rule.metric
               in resolvePercentSlab rule.slabs val
          )
          percentRules
      violNames =
        if null violationNames
          then violationMetricNamesPlaceholder
          else violationNames
      violPct =
        if null violationSlabs
          then Nothing
          else
            let totalInt =
                  sum $
                    map
                      (Evaluation.lookupMetricInt mbMetrics mempty . Evaluation.metricInsideKey)
                      violNames
             in Just (resolvePercentSlab violationSlabs totalInt)
      allPcts = metricPercents <> maybeToList violPct
   in case allPcts of
        [] -> Nothing
        xs -> Just (minimum xs)

-- | TODOCAMPAIGN: product will supply real violation metric names later.
violationMetricNamesPlaceholder :: [Text]
violationMetricNamesPlaceholder =
  [ "dxCancel",
    "dafCancel"
  ]
