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
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (Language (..), SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisLTSFlowEnv)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverPlan as SQPlan
import qualified Storage.Queries.Person as QPerson
import Tools.Encryption
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)
import qualified Tools.Payout as Payout

type JourneyRewardDispatchFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    Coins.EventFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

hasAssignedJourneys :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m Bool
hasAssignedJourneys personId = not . null <$> CQAssignment.findAssignmentsByUserId IJ.DriverActor (cast personId)

findAssignmentsByUserId :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findAssignmentsByUserId personId = CQAssignment.findAssignmentsByUserId IJ.DriverActor (cast personId)

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
    (Just $ CQMilestone.findByJourneyId IJ.DriverActor journeyId)

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
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone displayReward
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
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone 0
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
            loadEnabledJourneys = \mId cityId ->
              getConfig
                ( IncentiveJourneyDimensions
                    { merchantOperatingCityId = cityId.getId,
                      merchantId = Just mId.getId,
                      journeyId = Nothing,
                      enabled = Just True,
                      journeyType = Nothing
                    }
                )
                (Just $ CQJourney.findEnabledByMerchantIdAndMerchantOperatingCityId IJ.DriverActor mId cityId),
            loadMilestones = loadJourneyMilestones . cast,
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
              (Just ctx.milestoneId)
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
            vehCategory
            ("incentive journey cashback: " <> journey.name <> " milestone " <> ctx.milestoneId)
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
          let entityId = Just journey.id.getId
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
            vehCategory
            ("incentive streak-end cashback: " <> journey.name <> " campaignKey=" <> campaignKey)
            amount
    IJ.SubscriptionWaiveOff ->
      awardJourneySubscriptionWaiveOff driverId ("streak-end " <> journey.name <> " campaignKey=" <> campaignKey) spec
    IJ.NoReward -> pure (IJ.Awarded 0)
    otherKind -> do
      logInfo $ show otherKind <> " streak-end reward deferred journey=" <> journey.id.getId
      pure IJ.AwardSkipped

-- | Bank payout (same PG path as coin→direct payout) without burning coins.
-- Returns Awarded only when createPayoutService succeeds; otherwise AwardSkipped.
awardJourneyCashPayout ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DTV.VehicleCategory ->
  Text ->
  Int ->
  m IJ.AwardResult
awardJourneyCashPayout driverId merchantId merchantOpCityId transporterConfig vehCategory remark amountRupees = do
  result <-
    withTryCatch "IncentiveJourney:cashPayout" $ do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      payoutConfig <-
        getOneConfig
          ( PayoutConfigDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                vehicleCategory = Just vehCategory,
                isPayoutEnabled = Nothing
              }
          )
          Nothing
          >>= fromMaybeM (PayoutConfigNotFound (show vehCategory) merchantOpCityId.getId)
      unless payoutConfig.isPayoutEnabled $
        throwError $ InvalidRequest "Payout is not enabled for this vehicle category"
      uid <- generateGUID
      phoneNo <- mapM decrypt driver.mobileNumber
      driverInformation <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
      (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <-
        Payout.getCreatePayoutServiceFlow
          Payout.MerchantServiceUsageConfigOption
          DEMSC.PayoutService
          driver.clientSdkVersion
          merchantOpCityId
          driver.id
      vpa <- case payoutServiceFlow of
        IPayout.JuspayFlow -> Just <$> (driverInformation.payoutVpa & fromMaybeM (InvalidRequest "Driver has no payout VPA"))
        IPayout.StripeFlow -> pure Nothing
      merchantOperatingCity <-
        CQMOC.findById (cast merchantOpCityId)
          >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      let amount = fromIntegral amountRupees :: HighPrecMoney
          createPayoutOrderReq =
            DPayment.mkCreatePayoutServiceReq
              uid
              amount
              transporterConfig.currency
              phoneNo
              driver.email
              driverId.getId
              remark
              (Just driver.firstName)
              vpa
              payoutConfig.orderType
              payoutServiceFlow
              Nothing
          entityName = DPayment.INCENTIVE_JOURNEY_CASHBACK
          createPayoutOrderCall = Payout.createPayoutOrder payoutServiceName merchantOpCityId driver.id mbPersonBankAccount
      void $
        DPayment.createPayoutService
          (cast merchantId)
          (Just $ cast merchantOpCityId)
          (cast driverId)
          (Just [driverId.getId])
          (Just entityName)
          (show merchantOperatingCity.city)
          createPayoutOrderReq
          createPayoutOrderCall
          Nothing
          afterPayoutOrderCreated
      logInfo $
        "Created incentive journey cash payout order="
          <> uid
          <> " amount="
          <> show amountRupees
          <> " driver="
          <> driverId.getId
      pure amountRupees
  case result of
    Right awarded -> pure (IJ.Awarded awarded)
    Left _ -> pure IJ.AwardSkipped

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
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Maybe DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m ()
waiveDriverMilestone driverId merchantId merchantOpCityId transporterConfig journey milestoneId periodKey mbVehCategory mbServiceTier = do
  let vehCategory = fromMaybe DTV.CAR mbVehCategory
      ijHandle =
        IJ.IncentiveJourneyHandle
          { actor = IJ.DriverActor,
            loadEnabledJourneys = \_ _ -> pure [journey],
            loadMilestones = \cityId jId -> loadJourneyMilestones (cast cityId) jId,
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
      journey
      milestoneId
      periodKey
  when didWaive $ do
    milestones <- loadJourneyMilestones merchantOpCityId journey.id
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
