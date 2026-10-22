{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( hasAssignedJourneys,
    findAssignmentsByUserId,
    selectPreferredJourney,
    orderJourneysForDisplay,
    mkJourneyPeriodKey,
    mkWeeklyPeriodKey,
    mkMonthlyPeriodKey,
    mkDailyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    evaluateDriverJourney,
    loadJourneyMilestones,
    waiveDriverMilestone,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverPlan as DDriverPlan
import Domain.Types.Extra.Plan ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.External.Types (Language (..), SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisLTSFlowEnv)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Error.Types (FinanceError)
import qualified Lib.Finance.Ledger.Service as LedgerService
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import SharedLogic.Finance.Prepaid (counterpartyDriver)
import qualified SharedLogic.Finance.Wallet as SLFW
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverPlan as SQPlan
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)

type JourneyRewardDispatchFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Coins.EventFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

hasAssignedJourneys :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m Bool
hasAssignedJourneys personId = not . null <$> findAssignmentsByUserId personId

findAssignmentsByUserId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findAssignmentsByUserId personId = do
  mbPerson <- QPerson.findById personId
  mbVehicle <- QVeh.findById personId
  case mbPerson of
    Nothing -> CQAssignment.findAssignmentsByUserId IJ.DriverActor (cast personId)
    Just person ->
      CQAssignment.findAssignmentsForDriver
        IJ.DriverActor
        (cast personId)
        (cast person.merchantId)
        (cast person.merchantOperatingCityId)
        (T.pack . show <$> (mbVehicle >>= (.category)))

journeyTypeOrDefault :: Maybe DIJ.IncentiveJourneyType -> DIJ.IncentiveJourneyType
journeyTypeOrDefault = IJ.journeyTypeOrDefault

selectPreferredJourney :: UTCTime -> [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] -> Maybe DIJ.IncentiveJourney
selectPreferredJourney = IJ.selectPreferredJourney

orderJourneysForDisplay :: UTCTime -> [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] -> [DIJ.IncentiveJourney]
orderJourneysForDisplay = IJ.orderJourneysForDisplay

isJourneyWindowActive :: UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
isJourneyWindowActive = IJ.isJourneyWindowActiveFor

mkJourneyPeriodKey :: UTCTime -> DIJ.IncentiveJourney -> Text
mkJourneyPeriodKey = IJ.mkJourneyPeriodKeyFor

mkWeeklyPeriodKey :: UTCTime -> Text
mkWeeklyPeriodKey = IJ.mkWeeklyPeriodKey

mkMonthlyPeriodKey :: UTCTime -> Text
mkMonthlyPeriodKey = IJ.mkMonthlyPeriodKey

mkDailyPeriodKey :: UTCTime -> Text
mkDailyPeriodKey = IJ.mkDailyPeriodKey

loadJourneyMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
loadJourneyMilestones = CQMilestone.findByJourneyId IJ.DriverActor

sendMilestoneCompletedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  [DIJM.IncentiveJourneyMilestone] ->
  Int ->
  m ()
sendMilestoneCompletedOverlay merchantOpCityId driverId journey milestone journeyMilestones awarded = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mOverlay <-
    CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
      merchantOpCityId
      IJ.milestoneCompletedOverlayKey
      (fromMaybe ENGLISH driver.language)
      Nothing
      Nothing
      Nothing
  whenJust mOverlay $ \overlay -> do
    let milestoneTarget = IJ.resolveOverlayMilestoneDescription milestone journeyMilestones
        displayReward = IJ.displayMilestoneRewardAmount awarded milestone
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone journeyMilestones displayReward
        overlay' =
          overlay
            { DOverlay.title = fmap applyTemplates overlay.title,
              DOverlay.description = fmap applyTemplates overlay.description,
              DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
              DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
              DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
              DOverlay.actions = [IJ.milestoneCompletedOverlayKey]
            }
    sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'

sendMilestoneWaivedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
sendMilestoneWaivedOverlay merchantOpCityId driverId journey milestone journeyMilestones = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mOverlay <-
    CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
      merchantOpCityId
      IJ.milestoneWaivedOverlayKey
      (fromMaybe ENGLISH driver.language)
      Nothing
      Nothing
      Nothing
  whenJust mOverlay $ \overlay -> do
    let milestoneTarget = IJ.resolveOverlayMilestoneDescription milestone journeyMilestones
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone journeyMilestones 0
        overlay' =
          overlay
            { DOverlay.title = fmap applyTemplates overlay.title,
              DOverlay.description = fmap applyTemplates overlay.description,
              DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
              DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
              DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
              DOverlay.actions = [IJ.milestoneWaivedOverlayKey]
            }
    sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'

-- | EndRide journey evaluation. Call when driver has user_cohort_mapping rows.
evaluateDriverJourney ::
  (JourneyRewardDispatchFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  IncentiveMetrics.RideIncentiveDeltas ->
  m ()
evaluateDriverJourney driverId merchantId merchantOpCityId transporterConfig vehCategory mbServiceTier _mbEntityId mbPickupSpecialLocationId mbDropSpecialLocationId timeBoundReferenceUtc rideDeltas = do
  let ijHandle =
        IJ.IncentiveJourneyHandle
          { actor = IJ.DriverActor,
            loadJourneysByIds = CQJourney.findByIds IJ.DriverActor,
            loadMilestones = loadJourneyMilestones,
            dispatchReward = \journey ctx spec ->
              dispatchDriverReward
                driverId
                merchantId
                merchantOpCityId
                transporterConfig
                journey
                vehCategory
                mbServiceTier
                ctx
                spec,
            dispatchStreakEndReward = \journey campaignKey spec ->
              dispatchDriverStreakEndReward
                driverId
                merchantId
                merchantOpCityId
                transporterConfig
                journey
                vehCategory
                mbServiceTier
                campaignKey
                spec,
            mbOnMilestoneCompleted =
              Just $ \journey milestone journeyMilestones awarded ->
                void $
                  withTryCatch "IncentiveJourney:sendMilestoneCompletedOverlay" $
                    sendMilestoneCompletedOverlay
                      merchantOpCityId
                      driverId
                      journey
                      milestone
                      journeyMilestones
                      awarded
          }
      input =
        IJ.EvaluateInput
          { personId = cast driverId,
            merchantId = cast merchantId,
            merchantOperatingCityId = cast merchantOpCityId,
            timeBoundReferenceUtc = timeBoundReferenceUtc,
            timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
            rideDeltas = toLibDeltas rideDeltas,
            mbPickupSpecialLocationId = mbPickupSpecialLocationId,
            mbDropSpecialLocationId = mbDropSpecialLocationId,
            mbRideVehicleCategory = Just (show vehCategory),
            mbRideServiceTierType = show <$> mbServiceTier
          }
  IJ.evaluateIncentiveJourneys ijHandle input

dispatchDriverReward ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  IJ.RewardDispatchCtx ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchDriverReward driverId merchantId merchantOpCityId transporterConfig journey vehCategory mbServiceTier ctx spec =
  case spec.rewardKind of
    IJ.Coins ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $
            "Journey milestone "
              <> ctx.milestoneId
              <> " has Coins rewardType but no rewardValue; skipping award"
          pure IJ.AwardSkipped
        Just coinsToAward | coinsToAward <= 0 -> do
          logInfo $
            "Journey milestone "
              <> ctx.milestoneId
              <> " has non-positive rewardValue; skipping award"
          pure IJ.AwardSkipped
        Just coinsToAward -> do
          awarded <-
            Coins.awardJourneyMilestoneCoins
              driverId
              merchantId
              merchantOpCityId
              journey.name
              spec.rewardExpirationAt
              coinsToAward
              (Just (ctx.cohortJourneyMappingId <> ":" <> ctx.milestoneId))
              vehCategory
              mbServiceTier
          when (awarded > 0) $
            Coins.updateDriverCoins driverId awarded transporterConfig.timeDiffFromUtc
          logInfo $
            "Awarded "
              <> show awarded
              <> " coins for journey milestone "
              <> ctx.milestoneId
              <> " driver "
              <> driverId.getId
              <> " journey "
              <> journey.name
          if awarded > 0 then pure (IJ.Awarded awarded) else pure IJ.AwardSkipped
    IJ.Cash ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Journey milestone " <> ctx.milestoneId <> " Cash reward missing rewardValue; skipping"
          pure IJ.AwardSkipped
        Just amount | amount <= 0 -> do
          logInfo $ "Journey milestone " <> ctx.milestoneId <> " Cash reward non-positive; skipping"
          pure IJ.AwardSkipped
        Just amount ->
          awardJourneyCashPayout
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            (milestoneCashReferenceId driverId ctx)
            amount
    IJ.SubscriptionWaiveOff ->
      awardJourneySubscriptionWaiveOff driverId ("milestone " <> ctx.milestoneId <> " journey " <> journey.name) spec
    IJ.NoReward -> pure (IJ.Awarded 0)
    otherKind -> do
      logInfo $ show otherKind <> " reward deferred for journey milestone " <> ctx.milestoneId
      pure IJ.AwardSkipped

dispatchDriverStreakEndReward ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchDriverStreakEndReward driverId merchantId merchantOpCityId transporterConfig journey vehCategory mbServiceTier campaignKey spec =
  case spec.rewardKind of
    IJ.Coins ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Streak-end Coins reward missing rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just coinsToAward | coinsToAward <= 0 -> do
          logInfo $ "Streak-end non-positive rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just coinsToAward -> do
          let entityId = Just campaignKey
          awarded <-
            Coins.awardJourneyMilestoneCoins
              driverId
              merchantId
              merchantOpCityId
              journey.name
              spec.rewardExpirationAt
              coinsToAward
              entityId
              vehCategory
              mbServiceTier
          when (awarded > 0) $
            Coins.updateDriverCoins driverId awarded transporterConfig.timeDiffFromUtc
          logInfo $
            "Awarded "
              <> show awarded
              <> " streak-end coins driver "
              <> driverId.getId
              <> " journey "
              <> journey.name
              <> " campaignKey="
              <> campaignKey
          if awarded > 0 then pure (IJ.Awarded awarded) else pure IJ.AwardSkipped
    IJ.Cash ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Streak-end Cash reward missing rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just amount | amount <= 0 -> do
          logInfo $ "Streak-end Cash reward non-positive journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just amount ->
          awardJourneyCashPayout
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            (streakCashReferenceId driverId campaignKey)
            amount
    IJ.SubscriptionWaiveOff ->
      awardJourneySubscriptionWaiveOff driverId ("streak-end " <> journey.name <> " campaignKey=" <> campaignKey) spec
    IJ.NoReward -> pure (IJ.Awarded 0)
    otherKind -> do
      logInfo $ show otherKind <> " streak-end reward deferred journey=" <> journey.id.getId
      pure IJ.AwardSkipped

-- | Credit journey cash onto the driver wallet.
-- Returns Awarded only when the ledger credit exists; otherwise AwardSkipped so the milestone stays InProgress.
-- One lock per referenceId so two ride endings cannot both pass the ledger check and credit twice.
awardJourneyCashPayout ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Text ->
  Int ->
  m IJ.AwardResult
awardJourneyCashPayout driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees =
  Redis.withLockRedisAndReturnValue ("IncentiveJourneyCashLock:" <> referenceId) 10 $
    awardJourneyCashPayoutLocked driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees

awardJourneyCashPayoutLocked ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Text ->
  Int ->
  m IJ.AwardResult
awardJourneyCashPayoutLocked driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees = do
  existing <- LedgerService.getEntriesByReference SLFW.walletReferenceIncentiveJourneyCash referenceId
  if not (null existing)
    then do
      logInfo $
        "Incentive journey cash already in wallet referenceId="
          <> referenceId
          <> " driver="
          <> driverId.getId
      pure (IJ.Awarded amountRupees)
    else do
      let amount = fromIntegral amountRupees :: HighPrecMoney
      result <-
        SLFW.createWalletEntryDelta
          counterpartyDriver
          driverId.getId
          amount
          transporterConfig.currency
          merchantId.getId
          merchantOpCityId.getId
          SLFW.walletReferenceIncentiveJourneyCash
          referenceId
          Nothing
      case result of
        Right (_ :: HighPrecMoney) -> do
          logInfo $
            "Credited incentive journey cash to wallet referenceId="
              <> referenceId
              <> " amount="
              <> show amountRupees
              <> " driver="
              <> driverId.getId
          pure (IJ.Awarded amountRupees)
        Left (err :: FinanceError) -> do
          logError $
            "Incentive journey cash wallet credit failed referenceId="
              <> referenceId
              <> " driver="
              <> driverId.getId
              <> " error="
              <> show err
          pure IJ.AwardSkipped

milestoneCashReferenceId :: Id DP.Person -> IJ.RewardDispatchCtx -> Text
milestoneCashReferenceId driverId ctx =
  "milestone:"
    <> driverId.getId
    <> ":"
    <> ctx.cohortJourneyMappingId
    <> ":"
    <> ctx.milestoneId
    <> ":"
    <> ctx.periodKey

streakCashReferenceId :: Id DP.Person -> Text -> Text
streakCashReferenceId driverId campaignKey =
  "streak:" <> driverId.getId <> ":" <> campaignKey

-- | Applies dashboard subscription waive config. Missing config keeps the previous
-- hardcoded default (100%, 1 day, YATRI_SUBSCRIPTION, WITHOUT_OFFER).
awardJourneySubscriptionWaiveOff ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
awardJourneySubscriptionWaiveOff driverId contextLabel spec = do
  let waiveOff = fromMaybe defaultSubscriptionWaiveOff spec.subscriptionWaiveOff
  case (readMaybe (T.unpack waiveOff.serviceName), readMaybe (T.unpack waiveOff.waiveOffMode)) of
    (Nothing, _) -> do
      logInfo $ "SubscriptionWaiveOff skipped (invalid serviceName=" <> waiveOff.serviceName <> ") " <> contextLabel
      pure IJ.AwardSkipped
    (_, Nothing) -> do
      logInfo $ "SubscriptionWaiveOff skipped (invalid waiveOffMode=" <> waiveOff.waiveOffMode <> ") " <> contextLabel
      pure IJ.AwardSkipped
    (Just serviceName, Just waiveOfMode) -> do
      mbDriverPlan <- SQPlan.findByDriverIdAndServiceName driverId serviceName
      case mbDriverPlan of
        Nothing -> do
          logInfo $
            "SubscriptionWaiveOff skipped (no driver_plan service="
              <> waiveOff.serviceName
              <> ") "
              <> contextLabel
              <> " driver="
              <> driverId.getId
          pure IJ.AwardSkipped
        Just _ -> do
          SQPlan.updateWaiveOffPercantageAndType
            DDriverPlan.WaiveOffEntity
              { daysValidFor = fromIntegral waiveOff.daysValidFor,
                driverId = driverId.getId,
                percentage = fromIntegral waiveOff.percentage,
                serviceName = serviceName,
                waiveOfMode = waiveOfMode
              }
          logInfo $
            "SubscriptionWaiveOff applied ("
              <> show waiveOff.percentage
              <> "%, daysValidFor="
              <> show waiveOff.daysValidFor
              <> ", service="
              <> waiveOff.serviceName
              <> ", mode="
              <> waiveOff.waiveOffMode
              <> ") "
              <> contextLabel
              <> " driver="
              <> driverId.getId
          pure (IJ.Awarded waiveOff.percentage)

defaultSubscriptionWaiveOff :: IJ.SubscriptionWaiveOffSpec
defaultSubscriptionWaiveOff =
  IJ.SubscriptionWaiveOffSpec
    { percentage = 100,
      daysValidFor = 1,
      serviceName = "YATRI_SUBSCRIPTION",
      waiveOffMode = "WITHOUT_OFFER"
    }

-- | Dashboard waive-off for one driver milestone period row.
waiveDriverMilestone ::
  (JourneyRewardDispatchFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Maybe DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m ()
waiveDriverMilestone driverId merchantId merchantOpCityId transporterConfig journey cohortJourneyMappingId milestoneId periodKey mbVehCategory mbServiceTier = do
  let vehCategory = fromMaybe DTV.CAR mbVehCategory
      ijHandle =
        IJ.IncentiveJourneyHandle
          { actor = IJ.DriverActor,
            loadJourneysByIds = \_ -> pure [journey],
            loadMilestones = loadJourneyMilestones,
            dispatchReward = \j ctx spec ->
              dispatchDriverReward driverId merchantId merchantOpCityId transporterConfig j vehCategory mbServiceTier ctx spec,
            dispatchStreakEndReward = \j campaignKey spec ->
              dispatchDriverStreakEndReward driverId merchantId merchantOpCityId transporterConfig j vehCategory mbServiceTier campaignKey spec,
            mbOnMilestoneCompleted = Nothing
          }
  didWaive <-
    IJ.waiveMilestoneForPeriod
      ijHandle
      (cast driverId)
      (cast merchantId)
      (cast merchantOpCityId)
      (T.pack . show <$> mbVehCategory)
      journey
      cohortJourneyMappingId
      milestoneId
      periodKey
  when didWaive $ do
    milestones <- loadJourneyMilestones journey.id
    whenJust (find (\m -> m.id == milestoneId) milestones) $ \milestone ->
      void $
        withTryCatch "IncentiveJourney:sendMilestoneWaivedOverlay" $
          sendMilestoneWaivedOverlay merchantOpCityId driverId journey milestone milestones

toLibDeltas :: IncentiveMetrics.RideIncentiveDeltas -> IJ.RideDeltas
toLibDeltas deltas =
  IJ.RideDeltas
    { ridesDelta = deltas.ridesDelta,
      earningsDelta = deltas.earningsDelta,
      distanceMetersDelta = deltas.distanceMetersDelta,
      rideTimeSecondsDelta = deltas.rideTimeSecondsDelta
    }
