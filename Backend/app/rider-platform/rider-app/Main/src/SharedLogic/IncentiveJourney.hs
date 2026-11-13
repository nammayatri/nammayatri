{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( hasAssignedJourneys,
    findAssignmentsByUserId,
    findAssignmentsForPerson,
    mkJourneyPeriodKey,
    mkWeeklyPeriodKey,
    mkMonthlyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    evaluateRiderJourney,
    loadJourneyMilestones,
    waiveRiderMilestone,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified SharedLogic.Offer as SOffer
import Storage.Beam.IncentiveJourney ()
import qualified Storage.Queries.Person as QPerson
import Tools.Error

hasAssignedJourneys ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m Bool
hasAssignedJourneys personId merchantId merchantOpCityId mbRideVehicleCategory =
  not . null <$> findAssignmentsForPerson personId merchantId merchantOpCityId mbRideVehicleCategory

findAssignmentsForPerson ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m [IJ.JourneyAssignment]
findAssignmentsForPerson personId merchantId merchantOpCityId mbRideVehicleCategory =
  CQAssignment.findAssignmentsForPerson
    IJ.RiderActor
    (cast personId)
    (cast merchantId)
    (cast merchantOpCityId)
    mbRideVehicleCategory

findAssignmentsByUserId :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findAssignmentsByUserId personId = CQAssignment.findAssignmentsByUserId IJ.RiderActor (cast personId)

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
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
loadJourneyMilestones = CQMilestone.findByJourneyId IJ.RiderActor

mkRiderHandle ::
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  IJ.IncentiveJourneyHandle m
mkRiderHandle personId =
  IJ.IncentiveJourneyHandle
    { actor = IJ.RiderActor,
      loadJourneysByIds = CQJourney.findByIds IJ.RiderActor,
      loadMilestones = loadJourneyMilestones,
      dispatchReward = \_journey -> dispatchRiderReward personId,
      dispatchStreakEndReward = dispatchRiderStreakEndReward personId,
      mbOnMilestoneCompleted = Nothing
    }

evaluateRiderJourney ::
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  IJ.RideDeltas ->
  m ()
evaluateRiderJourney personId merchantId merchantOpCityId timeDiffFromUtc mbPickupSpecialLocationId mbDropSpecialLocationId mbRideVehicleCategory mbRideServiceTierType timeBoundReferenceUtc rideDeltas =
  IJ.evaluateIncentiveJourneys
    (mkRiderHandle personId)
    IJ.EvaluateInput
      { personId = cast personId,
        merchantId = cast merchantId,
        merchantOperatingCityId = cast merchantOpCityId,
        timeBoundReferenceUtc = timeBoundReferenceUtc,
        timeDiffFromUtc = timeDiffFromUtc,
        rideDeltas = rideDeltas,
        mbPickupSpecialLocationId = mbPickupSpecialLocationId,
        mbDropSpecialLocationId = mbDropSpecialLocationId,
        mbRideVehicleCategory = mbRideVehicleCategory,
        mbRideServiceTierType = mbRideServiceTierType
      }

waiveRiderMilestone ::
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m ()
waiveRiderMilestone personId merchantId merchantOpCityId journey cohortJourneyMappingId milestoneId periodKey =
  void $
    IJ.waiveMilestoneForPeriod
      (mkRiderHandle personId)
      (cast personId)
      (cast merchantId)
      (cast merchantOpCityId)
      Nothing
      journey
      cohortJourneyMappingId
      milestoneId
      periodKey

dispatchRiderReward ::
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  IJ.RewardDispatchCtx ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchRiderReward personId ctx spec = do
  case spec.rewardKind of
    IJ.Coupons ->
      case spec.coupon of
        Just couponSpec -> grantJourneyCoupon personId couponSpec ("milestone " <> ctx.milestoneId)
        Nothing -> do
          logError $ "Rider incentive journey Coupons reward has no offer code for milestone " <> ctx.milestoneId
          pure IJ.AwardSkipped
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
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchRiderStreakEndReward personId journey campaignKey spec = do
  case spec.rewardKind of
    IJ.Coupons ->
      case spec.coupon of
        Just couponSpec -> grantJourneyCoupon personId couponSpec ("streak-end " <> campaignKey)
        Nothing -> do
          logError $ "Rider streak-end Coupons reward has no offer code journey=" <> journey.id.getId <> " campaignKey=" <> campaignKey
          pure IJ.AwardSkipped
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

grantJourneyCoupon ::
  (Log m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  IJ.CouponSpec ->
  Text ->
  m IJ.AwardResult
grantJourneyCoupon personId couponSpec rewardSource = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  SOffer.addAutoApplyOfferTag person couponSpec.offerCode couponSpec.validityHours
  logInfo $
    "Rider incentive journey granted offer "
      <> couponSpec.offerCode
      <> " to person "
      <> personId.getId
      <> " for "
      <> rewardSource
  pure (IJ.Awarded 0)
