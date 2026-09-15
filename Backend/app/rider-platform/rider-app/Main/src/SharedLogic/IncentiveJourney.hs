{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( hasAssignedJourneys,
    findAssignmentsByUserId,
    mkJourneyPeriodKey,
    mkWeeklyPeriodKey,
    mkMonthlyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    evaluateRiderJourney,
    waiveRiderMilestone,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))

hasAssignedJourneys :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m Bool
hasAssignedJourneys personId = not . null <$> IJ.findAssignmentsByUserId (cast personId)

findAssignmentsByUserId :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findAssignmentsByUserId personId = IJ.findAssignmentsByUserId (cast personId)

journeyTypeOrDefault :: Maybe DIJ.IncentiveJourneyType -> DIJ.IncentiveJourneyType
journeyTypeOrDefault = IJ.journeyTypeOrDefault

isJourneyWindowActive :: UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
isJourneyWindowActive = IJ.isJourneyWindowActiveFor

mkJourneyPeriodKey :: UTCTime -> DIJ.IncentiveJourney -> Text
mkJourneyPeriodKey = IJ.mkJourneyPeriodKeyFor

mkWeeklyPeriodKey :: UTCTime -> Text
mkWeeklyPeriodKey = IJ.mkWeeklyPeriodKey

mkMonthlyPeriodKey :: UTCTime -> Text
mkMonthlyPeriodKey = IJ.mkMonthlyPeriodKey

loadJourneyMilestones ::
  (Log m, CacheFlow m r, EsqDBFlow m r) =>
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

mkRiderHandle ::
  (Log m, CacheFlow m r, EsqDBFlow m r) =>
  IJ.IncentiveJourneyHandle m
mkRiderHandle =
  IJ.IncentiveJourneyHandle
    { actor = IJ.RiderActor,
      loadEnabledJourneys = \merchantId cityId ->
        getConfig
          ( IncentiveJourneyDimensions
              { merchantOperatingCityId = cityId.getId,
                journeyId = Nothing,
                enabled = Just True
              }
          )
          (Just $ CQJourney.findEnabledByMerchantIdAndMerchantOperatingCityId (cast merchantId) (cast cityId)),
      loadMilestones = \cityId journeyId ->
        loadJourneyMilestones (cast cityId) journeyId,
      dispatchReward = \_journey -> dispatchRiderReward,
      dispatchStreakEndReward = dispatchRiderStreakEndReward,
      mbOnMilestoneCompleted = Nothing
    }

evaluateRiderJourney ::
  (Log m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  IJ.RideDeltas ->
  m ()
evaluateRiderJourney personId merchantId merchantOpCityId timeDiffFromUtc mbPickupSpecialLocationId mbDropSpecialLocationId timeBoundReferenceUtc rideDeltas =
  IJ.evaluateIncentiveJourneys
    mkRiderHandle
    IJ.EvaluateInput
      { personId = cast personId,
        merchantId = cast merchantId,
        merchantOperatingCityId = cast merchantOpCityId,
        timeBoundReferenceUtc = timeBoundReferenceUtc,
        timeDiffFromUtc = timeDiffFromUtc,
        rideDeltas = rideDeltas,
        mbPickupSpecialLocationId = mbPickupSpecialLocationId,
        mbDropSpecialLocationId = mbDropSpecialLocationId,
        mbRideVehicleCategory = Nothing,
        mbRideServiceTierType = Nothing
      }

waiveRiderMilestone ::
  (Log m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m ()
waiveRiderMilestone personId merchantId merchantOpCityId journey milestoneId periodKey =
  void $
    IJ.waiveMilestoneForPeriod
      mkRiderHandle
      (cast personId)
      (cast merchantId)
      (cast merchantOpCityId)
      journey
      milestoneId
      periodKey

dispatchRiderReward ::
  (Monad m, Log m) =>
  IJ.RewardDispatchCtx ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchRiderReward ctx spec = do
  case spec.rewardKind of
    IJ.Coins -> do
      logError $ "Rider incentive journey refused Coins reward for milestone " <> ctx.milestoneId
      pure IJ.AwardSkipped
    IJ.Cash -> do
      logError $ "Rider incentive journey refused Cash reward for milestone " <> ctx.milestoneId
      pure IJ.AwardSkipped
    IJ.NoReward -> pure $ IJ.Awarded 0
    otherKind -> do
      logInfo $ show otherKind <> " reward stubbed for rider journey milestone " <> ctx.milestoneId
      pure IJ.AwardSkipped

dispatchRiderStreakEndReward ::
  (Monad m, Log m) =>
  DIJ.IncentiveJourney ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchRiderStreakEndReward journey campaignKey spec = do
  case spec.rewardKind of
    IJ.Coins -> do
      logError $
        "Rider streak-end refused Coins reward journey="
          <> journey.id.getId
          <> " campaignKey="
          <> campaignKey
      pure IJ.AwardSkipped
    IJ.Cash -> do
      logError $
        "Rider streak-end refused Cash reward journey="
          <> journey.id.getId
          <> " campaignKey="
          <> campaignKey
      pure IJ.AwardSkipped
    IJ.NoReward -> pure $ IJ.Awarded 0
    otherKind -> do
      logInfo $
        show otherKind
          <> " streak-end reward stubbed journey="
          <> journey.id.getId
          <> " campaignKey="
          <> campaignKey
      pure IJ.AwardSkipped
