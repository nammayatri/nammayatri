{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( parseJourneyTags,
    hasJourneyTag,
    selectPreferredJourney,
    mkJourneyPeriodKey,
    mkWeeklyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    evaluateDriverJourney,
    loadJourneyMilestones,
  )
where

import qualified Data.Text as T
import Data.Time (utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.IncentiveJourneyStats as DIJS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Storage.CachedQueries.IncentiveJourneyStats as CQStats
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.IncentiveJourneyStats as QStats

-- | Parse Journey#<tag> values from driver tags. Value may contain "&"-separated segments.
parseJourneyTags :: Maybe [LYT.TagNameValueExpiry] -> [Text]
parseJourneyTags =
  concatMap parseOne . fromMaybe []
  where
    parseOne (LYT.TagNameValueExpiry rawTagText) =
      case T.splitOn "#" rawTagText of
        ("Journey" : tagValueText : _) ->
          filter (not . T.null) $ map T.strip (T.splitOn "&" tagValueText)
        _ -> []

hasJourneyTag :: Maybe [LYT.TagNameValueExpiry] -> Bool
hasJourneyTag = not . null . parseJourneyTags

journeyTypeOrDefault :: Maybe DIJ.IncentiveJourneyType -> DIJ.IncentiveJourneyType
journeyTypeOrDefault = fromMaybe DIJ.Daily

-- | Prefer a journey that is currently inside start/end date AND active timebound peak.
-- Else fall back to first matching journey (for "come back later" UI).
selectPreferredJourney :: UTCTime -> [DIJ.IncentiveJourney] -> Maybe DIJ.IncentiveJourney
selectPreferredJourney localTime journeys =
  case filter (isJourneyWindowActive localTime) journeys of
    (j : _) -> Just j
    [] -> listToMaybe journeys

isJourneyWindowActive :: UTCTime -> DIJ.IncentiveJourney -> Bool
isJourneyWindowActive localTime journey =
  let inDateRange = localTime >= journey.startDate && localTime <= journey.endDate
      inTimeBound = case journey.timeBounds of
        Just tb
          | tb /= TB.Unbounded ->
            case IncentiveMetrics.mkIncentiveWindowKey localTime tb of
              IncentiveMetrics.TimeBoundWindow _ -> True
              IncentiveMetrics.DayWindow -> False
        _ -> True
   in inDateRange && inTimeBound

-- | Stats uniqueness bucket for one evaluation window.
-- Daily (default): Day:YYYY-MM-DD or TimeBound:YYYY-MM-DD:<peak>
-- Weekly: Week:YYYY-Www (ISO week) — progress accumulates across the week in DB.
mkJourneyPeriodKey :: UTCTime -> DIJ.IncentiveJourney -> Text
mkJourneyPeriodKey localTime journey =
  case journeyTypeOrDefault journey.journeyType of
    DIJ.Weekly -> mkWeeklyPeriodKey localTime
    DIJ.Daily -> mkDailyPeriodKey localTime journey.timeBounds

mkDailyPeriodKey :: UTCTime -> Maybe TB.TimeBound -> Text
mkDailyPeriodKey localTime mbTimeBounds =
  let localDayText = T.pack (show (utctDay localTime))
   in case mbTimeBounds of
        Just tb
          | tb /= TB.Unbounded ->
            case IncentiveMetrics.mkIncentiveWindowKey localTime tb of
              IncentiveMetrics.TimeBoundWindow peakKey ->
                "TimeBound:" <> localDayText <> ":" <> peakKey
              IncentiveMetrics.DayWindow ->
                "Day:" <> localDayText
        _ -> "Day:" <> localDayText

mkWeeklyPeriodKey :: UTCTime -> Text
mkWeeklyPeriodKey localTime =
  let (year, week, _) = toWeekDate (utctDay localTime)
   in "Week:" <> T.pack (show year) <> "-W" <> T.pack (pad2 week)

pad2 :: Int -> String
pad2 n
  | n < 10 = '0' : show n
  | otherwise = show n

conditionOperatorOrDefault :: Maybe DIJM.MilestoneConditionOperator -> DIJM.MilestoneConditionOperator
conditionOperatorOrDefault = fromMaybe DIJM.GTE

evaluateCondition :: DIJM.MilestoneConditionOperator -> Int -> Int -> Bool
evaluateCondition conditionOperator lhs rhs =
  case conditionOperator of
    DIJM.GTE -> lhs >= rhs
    DIJM.GT -> lhs > rhs
    DIJM.EQ -> lhs == rhs
    DIJM.LTE -> lhs <= rhs
    DIJM.LT -> lhs < rhs
    DIJM.CT -> lhs >= rhs

deltaForCondition :: DIJM.MilestoneConditionType -> IncentiveMetrics.RideIncentiveDeltas -> Int
deltaForCondition conditionType deltas =
  case conditionType of
    DIJM.RideCompleted -> deltas.ridesDelta
    DIJM.Earnings -> deltas.earningsDelta
    DIJM.Distance -> deltas.distanceMetersDelta
    DIJM.RideDuration -> deltas.rideTimeSecondsDelta
    DIJM.PickupSpecialLocation -> deltas.ridesDelta
    DIJM.DropSpecialLocation -> deltas.ridesDelta
    DIJM.PickupDropSpecialLocation -> deltas.ridesDelta

rideMatchesLocationFilter :: DIJM.IncentiveJourneyMilestone -> Maybe Text -> Maybe Text -> Bool
rideMatchesLocationFilter milestone mbPickupSpecialLocationId mbDropSpecialLocationId =
  case milestone.conditionType of
    DIJM.PickupSpecialLocation ->
      isContainsOperator
        && matchesRequired milestone.pickupSpecialLocationIds mbPickupSpecialLocationId
        && matchesOptional milestone.dropSpecialLocationIds mbDropSpecialLocationId
    DIJM.DropSpecialLocation ->
      isContainsOperator
        && matchesRequired milestone.dropSpecialLocationIds mbDropSpecialLocationId
        && matchesOptional milestone.pickupSpecialLocationIds mbPickupSpecialLocationId
    DIJM.PickupDropSpecialLocation ->
      isContainsOperator
        && matchesRequired milestone.pickupSpecialLocationIds mbPickupSpecialLocationId
        && matchesRequired milestone.dropSpecialLocationIds mbDropSpecialLocationId
    _ ->
      matchesOptional milestone.pickupSpecialLocationIds mbPickupSpecialLocationId
        && matchesOptional milestone.dropSpecialLocationIds mbDropSpecialLocationId
  where
    isContainsOperator = conditionOperatorOrDefault milestone.conditionOperator == DIJM.CT
    matchesOptional Nothing _ = True
    matchesOptional (Just allowedIds) mbActualId = maybe False (`elem` allowedIds) mbActualId
    matchesRequired Nothing _ = False
    matchesRequired (Just allowedIds) mbActualId = maybe False (`elem` allowedIds) mbActualId

isMilestoneAlreadyCompleted :: Maybe DIJS.IncentiveJourneyStats -> Bool
isMilestoneAlreadyCompleted =
  maybe False ((`elem` [DIJS.Completed, DIJS.Rewarded]) . (.status))

loadJourneyMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
loadJourneyMilestones merchantOpCityId journeyId =
  getConfig
    ( IncentiveJourneyMilestoneDimensions
        { merchantOperatingCityId = merchantOpCityId.getId,
          journeyId = Just journeyId,
          milestoneId = Nothing
        }
    )
    (Just $ CQMilestone.findByJourneyId journeyId)

-- | EndRide journey evaluation. Call only when driver has a Journey# tag.
-- Progress is stored in incentive_journey_stats (DB); does not read Redis metrics.
evaluateDriverJourney ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Coins.EventFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Maybe [LYT.TagNameValueExpiry] ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  IncentiveMetrics.RideIncentiveDeltas ->
  m ()
evaluateDriverJourney driverId merchantId merchantOpCityId transporterConfig driverTag vehCategory mbServiceTier mbEntityId mbPickupSpecialLocationId mbDropSpecialLocationId timeBoundReferenceUtc rideDeltas = do
  let journeyTags = parseJourneyTags driverTag
  when (null journeyTags) $
    logInfo $ "evaluateDriverJourney called with no Journey tags for driver " <> driverId.getId
  unless (null journeyTags) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) timeBoundReferenceUtc
    enabledJourneys <-
      getConfig
        ( IncentiveJourneyDimensions
            { merchantOperatingCityId = merchantOpCityId.getId,
              journeyId = Nothing,
              enabled = Just True,
              vehicleCategory = Nothing
            }
        )
        (Just $ CQJourney.findEnabledByMerchantOperatingCityId merchantOpCityId)
    let matching =
          filter
            ( \j ->
                j.merchantId == merchantId
                  && j.driverTag `elem` journeyTags
                  && (isNothing j.vehicleCategory || j.vehicleCategory == Just vehCategory)
            )
            enabledJourneys
    case selectPreferredJourney localTime matching of
      Nothing ->
        logInfo $ "No matching IncentiveJourney for driver " <> driverId.getId <> " tags=" <> show journeyTags
      Just journey -> do
        if not (isJourneyWindowActive localTime journey)
          then
            logInfo $
              "IncentiveJourney "
                <> journey.id.getId
                <> " matched but outside active window; skipping evaluation"
          else do
            let periodKey = mkJourneyPeriodKey localTime journey
            milestones <- loadJourneyMilestones merchantOpCityId journey.id
            ensureMilestoneStatsRows driverId merchantId merchantOpCityId journey.id periodKey milestones
            logInfo $
              "Evaluating IncentiveJourney "
                <> journey.id.getId
                <> " journeyType="
                <> show (journeyTypeOrDefault journey.journeyType)
                <> " periodKey="
                <> periodKey
                <> " milestones="
                <> show (length milestones)
            evaluateMilestonesInOrder
              driverId
              merchantId
              merchantOpCityId
              transporterConfig
              journey
              periodKey
              rideDeltas
              mbPickupSpecialLocationId
              mbDropSpecialLocationId
              vehCategory
              mbServiceTier
              mbEntityId
              milestones

-- | Create NotStarted / 0 rows for milestones missing stats in this period.
ensureMilestoneStatsRows ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DIJ.IncentiveJourney ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
ensureMilestoneStatsRows driverId merchantId merchantOpCityId journeyId periodKey milestones =
  forM_ milestones $ \milestone -> do
    mbExisting <- QStats.findStatsByDriverAndMilestonePeriod driverId journeyId milestone.id periodKey
    when (isNothing mbExisting) $ do
      now <- getCurrentTime
      statsId <- generateGUID
      void $
        CQStats.upsertJourneyStats
          DIJS.IncentiveJourneyStats
            { id = statsId,
              driverId = driverId,
              journeyId = journeyId,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = 0,
              status = DIJS.NotStarted,
              rewardType = milestone.rewardType,
              rewardValue = Nothing,
              createdAt = now,
              updatedAt = now,
              merchantId = Just merchantId,
              merchantOperatingCityId = Just merchantOpCityId
            }

evaluateMilestonesInOrder ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Coins.EventFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  Text ->
  IncentiveMetrics.RideIncentiveDeltas ->
  Maybe Text ->
  Maybe Text ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Maybe Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
evaluateMilestonesInOrder _ _ _ _ _ _ _ _ _ _ _ _ [] = pure ()
evaluateMilestonesInOrder driverId merchantId merchantOpCityId transporterConfig journey periodKey rideDeltas mbPickupSpecialLocationId mbDropSpecialLocationId vehCategory mbServiceTier mbEntityId (milestone : rest) = do
  mbExisting <- QStats.findStatsByDriverAndMilestonePeriod driverId journey.id milestone.id periodKey
  if isMilestoneAlreadyCompleted mbExisting
    then evaluateMilestonesInOrder driverId merchantId merchantOpCityId transporterConfig journey periodKey rideDeltas mbPickupSpecialLocationId mbDropSpecialLocationId vehCategory mbServiceTier mbEntityId rest
    else
      if not (rideMatchesLocationFilter milestone mbPickupSpecialLocationId mbDropSpecialLocationId)
        then pure ()
        else evaluateMatchingMilestone mbExisting
  where
    evaluateMatchingMilestone mbExisting = do
      let delta = deltaForCondition milestone.conditionType rideDeltas
          prevValue = maybe 0 (.currentValue) mbExisting
          currentValue = prevValue + delta
          conditionMet =
            evaluateCondition
              (conditionOperatorOrDefault milestone.conditionOperator)
              currentValue
              milestone.conditionValue
      now <- getCurrentTime
      statsId <- maybe generateGUID (pure . (.id)) mbExisting
      let baseStats =
            DIJS.IncentiveJourneyStats
              { id = statsId,
                driverId = driverId,
                journeyId = journey.id,
                milestoneId = milestone.id,
                periodKey = periodKey,
                conditionType = milestone.conditionType,
                conditionOperator = milestone.conditionOperator,
                conditionValue = milestone.conditionValue,
                currentValue = currentValue,
                status = DIJS.InProgress,
                rewardType = milestone.rewardType,
                rewardValue = Nothing,
                createdAt = maybe now (.createdAt) mbExisting,
                updatedAt = now,
                merchantId = Just merchantId,
                merchantOperatingCityId = Just merchantOpCityId
              }
      if not conditionMet
        then do
          void $ CQStats.upsertJourneyStats baseStats {DIJS.status = DIJS.InProgress, DIJS.rewardValue = Nothing}
          pure ()
        else do
          awarded <-
            awardMilestoneReward
              driverId
              merchantId
              merchantOpCityId
              transporterConfig
              milestone
              vehCategory
              mbServiceTier
              mbEntityId
          let shouldComplete =
                case milestone.rewardType of
                  DIJM.Coins -> awarded > 0
                  DIJM.Cash -> True
                  DIJM.Coupons -> True
          if shouldComplete
            then do
              void $
                CQStats.upsertJourneyStats
                  baseStats
                    { DIJS.status = DIJS.Completed,
                      DIJS.rewardValue = Just awarded
                    }
              evaluateMilestonesInOrder driverId merchantId merchantOpCityId transporterConfig journey periodKey rideDeltas mbPickupSpecialLocationId mbDropSpecialLocationId vehCategory mbServiceTier mbEntityId rest
            else do
              void $ CQStats.upsertJourneyStats baseStats {DIJS.status = DIJS.InProgress, DIJS.rewardValue = Nothing}
              pure ()

awardMilestoneReward ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Coins.EventFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJM.IncentiveJourneyMilestone ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Maybe Text ->
  m Int
awardMilestoneReward driverId merchantId merchantOpCityId transporterConfig milestone vehCategory mbServiceTier mbEntityId =
  case milestone.rewardType of
    DIJM.Coins ->
      case milestone.rewardConfigId of
        Nothing -> do
          logInfo $
            "Journey milestone "
              <> milestone.id.getId
              <> " has Coins rewardType but no rewardConfigId; skipping award"
          pure 0
        Just configId -> do
          mbConfig <- SQCC.findById configId
          case mbConfig of
            Nothing -> do
              logError $
                "CoinsConfig "
                  <> configId.getId
                  <> " not found for journey milestone "
                  <> milestone.id.getId
              pure 0
            Just coinsConfig -> do
              let coinsToAward = coinsConfig.coins
              awarded <-
                Coins.updateEventAndGetCoinsvalue
                  driverId
                  merchantId
                  merchantOpCityId
                  coinsConfig.eventFunction
                  coinsConfig.expirationAt
                  coinsToAward
                  mbEntityId
                  vehCategory
                  mbServiceTier
              when (awarded > 0) $
                Coins.updateDriverCoins driverId awarded transporterConfig.timeDiffFromUtc
              logInfo $
                "Awarded "
                  <> show awarded
                  <> " coins for journey milestone "
                  <> milestone.id.getId
                  <> " driver "
                  <> driverId.getId
                  <> " from coinsConfig "
                  <> configId.getId
              pure awarded
    DIJM.Cash -> do
      logInfo $ "Cash reward deferred for journey milestone " <> milestone.id.getId
      pure 0
    DIJM.Coupons -> do
      logInfo $ "Coupons reward deferred for journey milestone " <> milestone.id.getId
      pure 0
