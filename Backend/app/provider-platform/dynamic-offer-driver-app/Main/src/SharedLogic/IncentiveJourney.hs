{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( parseJourneyTags,
    hasJourneyTag,
    selectPreferredJourney,
    orderJourneysForDisplay,
    mkJourneyPeriodKey,
    mkWeeklyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    matchesJourneyVehicle,
    matchesJourneyVehicleForDriver,
    evaluateDriverJourney,
    loadJourneyMilestones,
  )
where

import Data.List (partition)
import qualified Data.Text as T
import Data.Time (utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.IncentiveJourneyStats as DIJS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisLTSFlowEnv)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.SpecialZone ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import qualified Storage.Queries.IncentiveJourneyStats as QStats
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)

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

selectPreferredJourney :: UTCTime -> [DIJ.IncentiveJourney] -> Maybe DIJ.IncentiveJourney
selectPreferredJourney localTime = listToMaybe . orderJourneysForDisplay localTime

orderJourneysForDisplay :: UTCTime -> [DIJ.IncentiveJourney] -> [DIJ.IncentiveJourney]
orderJourneysForDisplay localTime journeys =
  let (active, inactive) = partition (isJourneyWindowActive localTime) journeys
   in active <> inactive

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

matchesJourneyVehicle :: DIJ.IncentiveJourney -> DTV.VehicleCategory -> Maybe DCommon.ServiceTierType -> Bool
matchesJourneyVehicle journey vehCategory mbServiceTier =
  (isNothing journey.vehicleCategory || journey.vehicleCategory == Just vehCategory)
    && (isNothing journey.serviceTierType || journey.serviceTierType == mbServiceTier)

matchesJourneyVehicleForDriver :: DIJ.IncentiveJourney -> DTV.VehicleCategory -> [DCommon.ServiceTierType] -> Bool
matchesJourneyVehicleForDriver journey vehCategory selectedServiceTiers =
  (isNothing journey.vehicleCategory || journey.vehicleCategory == Just vehCategory)
    && ( isNothing journey.serviceTierType
           || maybe False (`elem` selectedServiceTiers) journey.serviceTierType
       )

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

milestoneCompletedOverlayKey :: Text
milestoneCompletedOverlayKey = "INCENTIVE_JOURNEY_MILESTONE_COMPLETED"

overlayTemplateText :: Text -> Text
overlayTemplateText txt = "{#" <> txt <> "#}"

formatRidesCompleted :: Int -> Maybe Text -> Text
formatRidesCompleted n mbQualifier =
  let rideWord = if n == 1 then "ride" else "rides"
   in case mbQualifier of
        Nothing -> show n <> " " <> rideWord <> " completed"
        Just qualifier -> show n <> " " <> qualifier <> " " <> rideWord <> " completed"

formatEarningsCompleted :: Int -> Text
formatEarningsCompleted n = "Rs " <> show n <> " earned"

formatDistanceCompleted :: Int -> Text
formatDistanceCompleted meters
  | meters >= 1000 && meters `mod` 1000 == 0 =
    show (meters `div` 1000) <> " km covered"
  | otherwise =
    show meters <> " m covered"

formatDurationCompleted :: Int -> Text
formatDurationCompleted seconds
  | seconds >= 3600 && seconds `mod` 3600 == 0 =
    show (seconds `div` 3600) <> " hr completed"
  | seconds >= 60 && seconds `mod` 60 == 0 =
    show (seconds `div` 60) <> " min completed"
  | otherwise =
    show seconds <> " sec completed"

resolveSpecialLocationNameFromId ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Maybe Text ->
  m Text
resolveSpecialLocationNameFromId = \case
  Nothing -> pure "special location"
  Just slIdText -> do
    mbSpecialLocation <- QSpecialLocation.findById (Id slIdText)
    pure $ maybe slIdText (.locationName) mbSpecialLocation

buildMilestoneTargetDescription ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Maybe Text ->
  Maybe Text ->
  DIJM.IncentiveJourneyMilestone ->
  m Text
buildMilestoneTargetDescription mbRidePickupSpecialLocationId mbRideDropSpecialLocationId milestone =
  case milestone.conditionType of
    DIJM.RideCompleted ->
      pure $ formatRidesCompleted milestone.conditionValue Nothing
    DIJM.Earnings ->
      pure $ formatEarningsCompleted milestone.conditionValue
    DIJM.Distance ->
      pure $ formatDistanceCompleted milestone.conditionValue
    DIJM.RideDuration ->
      pure $ formatDurationCompleted milestone.conditionValue
    DIJM.PickupSpecialLocation -> do
      pickupLabel <- resolveSpecialLocationNameFromId mbRidePickupSpecialLocationId
      pure $ formatRidesCompleted milestone.conditionValue (Just pickupLabel)
    DIJM.DropSpecialLocation -> do
      dropLabel <- resolveSpecialLocationNameFromId mbRideDropSpecialLocationId
      pure $ formatRidesCompleted milestone.conditionValue (Just dropLabel)
    DIJM.PickupDropSpecialLocation -> do
      pickupLabel <- resolveSpecialLocationNameFromId mbRidePickupSpecialLocationId
      dropLabel <- resolveSpecialLocationNameFromId mbRideDropSpecialLocationId
      pure $ formatRidesCompleted milestone.conditionValue (Just $ pickupLabel <> " to " <> dropLabel)

applyMilestoneOverlayTemplates :: DIJ.IncentiveJourney -> Text -> DIJM.IncentiveJourneyMilestone -> Int -> Text -> Text
applyMilestoneOverlayTemplates journey milestoneTarget milestone displayReward txt =
  T.replace (overlayTemplateText "journeyName") journey.name
    . T.replace (overlayTemplateText "milestoneDescription") milestoneTarget
    . T.replace (overlayTemplateText "milestoneOrder") (show milestone.order)
    . T.replace (overlayTemplateText "rewardAmount") (show displayReward)
    . T.replace (overlayTemplateText "rewardType") (show milestone.rewardType)
    $ txt

displayMilestoneRewardAmount :: Int -> DIJM.IncentiveJourneyMilestone -> Int
displayMilestoneRewardAmount awarded milestone =
  if awarded > 0 then awarded else fromMaybe 0 milestone.rewardValue

sendMilestoneCompletedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  Maybe Text ->
  Maybe Text ->
  Int ->
  m ()
sendMilestoneCompletedOverlay merchantOpCityId driverId journey milestone mbRidePickupSpecialLocationId mbRideDropSpecialLocationId awarded = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mOverlay <-
    CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
      merchantOpCityId
      milestoneCompletedOverlayKey
      (fromMaybe ENGLISH driver.language)
      Nothing
      Nothing
      Nothing
  whenJust mOverlay $ \overlay -> do
    milestoneTarget <- buildMilestoneTargetDescription mbRidePickupSpecialLocationId mbRideDropSpecialLocationId milestone
    let displayReward = displayMilestoneRewardAmount awarded milestone
        applyTemplates = applyMilestoneOverlayTemplates journey milestoneTarget milestone displayReward
        overlay' =
          overlay
            { DOverlay.title = fmap applyTemplates overlay.title,
              DOverlay.description = fmap applyTemplates overlay.description,
              DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
              DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
              DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
              DOverlay.actions = [milestoneCompletedOverlayKey]
            }
    sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'

-- | EndRide journey evaluation. Call only when driver has a Journey# tag.
evaluateDriverJourney ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, Coins.EventFlow m r, HedisLTSFlowEnv r) =>
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
              vehicleCategory = Nothing,
              serviceTierType = Nothing
            }
        )
        (Just $ CQJourney.findEnabledByMerchantOperatingCityId merchantOpCityId)
    let matching =
          filter
            ( \j ->
                j.merchantId == merchantId
                  && j.driverTag `elem` journeyTags
                  && matchesJourneyVehicle j vehCategory mbServiceTier
            )
            enabledJourneys
        activeMatching = filter (isJourneyWindowActive localTime) matching
    case activeMatching of
      [] ->
        if null matching
          then logInfo $ "No matching IncentiveJourney for driver " <> driverId.getId <> " tags=" <> show journeyTags
          else
            logInfo $
              "IncentiveJourney(s) matched for driver "
                <> driverId.getId
                <> " but outside active window; skipping evaluation tags="
                <> show journeyTags
      journeysToEvaluate ->
        forM_ journeysToEvaluate $ \journey -> do
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
          void $
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
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, Coins.EventFlow m r, HedisLTSFlowEnv r) =>
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
              journey
              milestone
              vehCategory
              mbServiceTier
          let shouldComplete = awarded > 0
          if shouldComplete
            then do
              void $
                CQStats.upsertJourneyStats
                  baseStats
                    { DIJS.status = DIJS.Completed,
                      DIJS.rewardValue = Just awarded
                    }
              void $
                withTryCatch "IncentiveJourney:sendMilestoneCompletedOverlay" $
                  sendMilestoneCompletedOverlay merchantOpCityId driverId journey milestone mbPickupSpecialLocationId mbDropSpecialLocationId awarded
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
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m Int
awardMilestoneReward driverId merchantId merchantOpCityId transporterConfig journey milestone vehCategory mbServiceTier =
  case milestone.rewardType of
    DIJM.Coins ->
      case milestone.rewardValue of
        Nothing -> do
          logInfo $
            "Journey milestone "
              <> milestone.id.getId
              <> " has Coins rewardType but no rewardValue; skipping award"
          pure 0
        Just coinsToAward | coinsToAward <= 0 -> do
          logInfo $
            "Journey milestone "
              <> milestone.id.getId
              <> " has non-positive rewardValue; skipping award"
          pure 0
        Just coinsToAward -> do
          awarded <-
            Coins.awardJourneyMilestoneCoins
              driverId
              merchantId
              merchantOpCityId
              journey.name
              milestone.rewardExpirationAt
              coinsToAward
              (Just milestone.id.getId)
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
              <> " journey "
              <> journey.name
          pure awarded
    DIJM.Cash -> do
      logInfo $ "Cash reward deferred for journey milestone " <> milestone.id.getId
      pure 0
    DIJM.Coupons -> do
      logInfo $ "Coupons reward deferred for journey milestone " <> milestone.id.getId
      pure 0
