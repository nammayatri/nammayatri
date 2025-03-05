{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Merchant
  ( postMerchantServiceConfigMapsUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantUpdate,
    getMerchantServiceUsageConfig,
    postMerchantServiceConfigSmsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantServiceConfigVerificationUpdate,
    postMerchantConfigFarePolicyUpsert,
    postMerchantConfigOperatingCityCreate,
    postMerchantUpdateOnboardingVehicleVariantMapping,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    getMerchantConfigCommon,
    postMerchantConfigCommonUpdate,
    getMerchantConfigDriverPool,
    postMerchantConfigDriverPoolUpdate,
    postMerchantConfigDriverPoolCreate,
    getMerchantConfigDriverIntelligentPool,
    postMerchantConfigDriverIntelligentPoolUpdate,
    getMerchantConfigOnboardingDocument,
    postMerchantConfigOnboardingDocumentUpdate,
    postMerchantConfigOnboardingDocumentCreate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate,
    postMerchantConfigFarePolicyPerExtraKmRateUpdate,
    postMerchantConfigFarePolicyUpdate,
    postMerchantSchedulerTrigger,
    postMerchantConfigClearCacheSubscription,
    postMerchantConfigFailover,
    postMerchantPayoutConfigUpdate,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (DayOfWeek (..))
import qualified Data.Vector as V
import qualified Domain.Action.UI.MerchantServiceConfig as DMSC
import Domain.Action.UI.Ride.EndRide.Internal (setDriverFeeBillNumberKey, setDriverFeeCalcJobCache)
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.CancellationFarePolicy as DTCFP
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverIntelligentPoolConfig as DDIPC
import qualified Domain.Types.DriverPoolConfig as DDPC
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FarePolicy as FarePolicy
import qualified Domain.Types.FarePolicy.Common as FarePolicy
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DFPEFB
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as FPIDPS
import qualified Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer as FPRDDB
import qualified Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as FPRDPS
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.GoHomeConfig as DGoHomeConfig
import qualified Domain.Types.LeaderBoardConfigs as DLC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantPushNotification as DMPN
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Overlay as DMO
import qualified Domain.Types.PayoutConfig as DPC
import qualified Domain.Types.Plan as Plan
import qualified Domain.Types.SubscriptionConfig as DSC
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified EulerHS.Language as L
import qualified "shared-services" IssueManagement.Common as ICommon
import qualified "shared-services" IssueManagement.Domain.Types.Issue.IssueConfig as DIConfig
import qualified "shared-services" IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQIssueConfig
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Types.Registry (SimpleLookupRequest (..), lookupRequestToRedisKey)
import qualified Kernel.Types.Registry.Subscriber as BecknSub
import Kernel.Types.TimeBound as TB
import Kernel.Types.Value (MandatoryValue, OptionalValue)
import Kernel.Utils.Common
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Validation
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import qualified Registry.Beckn.Interface as RegistryIF
import qualified Registry.Beckn.Interface.Types as RegistryT
import SharedLogic.Allocator (AllocatorJobType (..), BadDebtCalculationJobData, CalculateDriverFeesJobData, DriverReferralPayoutJobData, SupplyDemandRequestJobData)
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as DriverPool
import qualified SharedLogic.DriverFee as SDF
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.IssueManagement ()
import qualified Storage.Cac.DriverIntelligentPoolConfig as CDIPC
import qualified Storage.Cac.DriverIntelligentPoolConfig as CQDIPC
import qualified Storage.Cac.DriverPoolConfig as CQDPC
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.GoHomeConfig as CQGHC
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.FareProduct as CQFProduct
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.LeaderBoardConfig as CQLBC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CQMPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.Overlay as CQMO
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.Plan as CQPlan
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.CancellationFarePolicy as QCFP
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QFPEFB
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as QFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QFPPDEKM
import qualified Storage.Queries.FareProduct as SQF
import qualified Storage.Queries.Geometry as QGEO
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.PayoutConfig as QPC
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.SubscriptionConfig as QSC
import Tools.Error

---------------------------------------------------------------------
postMerchantUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantUpdateReq -> Flow Common.MerchantUpdateRes
postMerchantUpdate merchantShortId opCity req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.description = req.description <|> merchant.description,
                 DM.enabled = fromMaybe merchant.enabled req.enabled
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantOperatingCityId /= merchantOpCityId) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  _ <- CQM.update updMerchant
  whenJust req.exoPhones \exophones -> do
    CQExophone.deleteByMerchantOpCityId merchantOpCityId
    forM_ exophones $ \exophoneReq -> do
      exophone <- buildExophone merchant.id merchantOpCityId now exophoneReq
      CQExophone.create exophone
  whenJust req.fcmConfig $
    \fcmConfig -> CQTC.updateFCMConfig merchantOpCityId fcmConfig.fcmUrl fcmConfig.fcmServiceAccount

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantOperatingCityId == merchantOpCityId) allExophones
    CQExophone.clearCache merchantOpCityId oldExophones
  whenJust req.fcmConfig $ \_ -> CQTC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantUpdate : " (show merchant.id)
  return $ mkMerchantUpdateRes updMerchant
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId merchantOpCityId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
        merchantOperatingCityId = merchantOpCityId,
        primaryPhone = req.primaryPhone,
        backupPhone = req.backupPhone,
        isPrimaryDown = False,
        exophoneType = DExophone.CALL_RIDE,
        callService = req.callService,
        updatedAt = now,
        createdAt = now
      }

mkMerchantUpdateRes :: DM.Merchant -> Common.MerchantUpdateRes
mkMerchantUpdateRes DM.Merchant {..} =
  Common.MerchantUpdateRes
    { name,
      description = description,
      contactNumber = mobileCountryCode <> mobileNumber,
      status = castMerchantStatus status,
      enabled = enabled
    }

castMerchantStatus :: DM.Status -> Common.Status
castMerchantStatus = \case
  DM.PENDING_VERIFICATION -> Common.PENDING_VERIFICATION
  DM.APPROVED -> Common.APPROVED
  DM.REJECTED -> Common.REJECTED

---------------------------------------------------------------------
getMerchantConfigCommon :: ShortId DM.Merchant -> Context.City -> Flow Common.MerchantCommonConfigRes
getMerchantConfigCommon merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  pure $ mkMerchantCommonConfigRes config

mkMerchantCommonConfigRes :: DTC.TransporterConfig -> Common.MerchantCommonConfigRes
mkMerchantCommonConfigRes DTC.TransporterConfig {..} =
  Common.MerchantCommonConfigRes
    { pickupLocThresholdWithUnit = convertMetersToDistance distanceUnit pickupLocThreshold,
      dropLocThresholdWithUnit = convertMetersToDistance distanceUnit dropLocThreshold,
      actualRideDistanceDiffThresholdWithUnit = convertHighPrecMetersToDistance distanceUnit actualRideDistanceDiffThreshold,
      upwardsRecomputeBufferWithUnit = convertHighPrecMetersToDistance distanceUnit upwardsRecomputeBuffer,
      approxRideDistanceDiffThresholdWithUnit = convertHighPrecMetersToDistance distanceUnit approxRideDistanceDiffThreshold,
      ..
    }

---------------------------------------------------------------------
mkDistanceField :: Meters -> Maybe (MandatoryValue Distance) -> Maybe (MandatoryValue Meters) -> Meters
mkDistanceField oldField reqFieldWithUnit reqField =
  fromMaybe oldField $
    (distanceToMeters . (.value) <$> reqFieldWithUnit)
      <|> ((.value) <$> reqField)

mkOptionalDistanceField :: Maybe Meters -> Maybe (OptionalValue Distance) -> Maybe (OptionalValue Meters) -> Maybe Meters
mkOptionalDistanceField oldField reqFieldWithUnit reqField =
  fromMaybe oldField $
    ((distanceToMeters <$>) . (.value) <$> reqFieldWithUnit)
      <|> ((.value) <$> reqField)

postMerchantConfigCommonUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantCommonConfigUpdateReq -> Flow APISuccess
postMerchantConfigCommonUpdate merchantShortId opCity req = do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let updConfig =
        config{pickupLocThreshold = mkDistanceField config.pickupLocThreshold req.pickupLocThresholdWithUnit req.pickupLocThreshold,
               dropLocThreshold = mkDistanceField config.dropLocThreshold req.dropLocThresholdWithUnit req.dropLocThreshold,
               rideTimeEstimatedThreshold = maybe config.rideTimeEstimatedThreshold (.value) req.rideTimeEstimatedThreshold,
               defaultPopupDelay = maybe config.defaultPopupDelay (.value) req.defaultPopupDelay,
               popupDelayToAddAsPenalty = maybe config.popupDelayToAddAsPenalty (.value) req.popupDelayToAddAsPenalty,
               thresholdCancellationScore = maybe config.thresholdCancellationScore (.value) req.thresholdCancellationScore,
               minRidesForCancellationScore = maybe config.minRidesForCancellationScore (.value) req.minRidesForCancellationScore,
               mediaFileUrlPattern = maybe config.mediaFileUrlPattern (.value) req.mediaFileUrlPattern,
               mediaFileSizeUpperLimit = maybe config.mediaFileSizeUpperLimit (.value) req.mediaFileSizeUpperLimit,
               onboardingTryLimit = maybe config.onboardingTryLimit (.value) req.onboardingTryLimit,
               onboardingRetryTimeInHours = maybe config.onboardingRetryTimeInHours (.value) req.onboardingRetryTimeInHours,
               checkImageExtractionForDashboard = maybe config.checkImageExtractionForDashboard (.value) req.checkImageExtractionForDashboard,
               searchRepeatLimit = maybe config.searchRepeatLimit (.value) req.searchRepeatLimit,
               driverPaymentCycleBuffer = maybe config.driverPaymentCycleBuffer (.value) req.driverPaymentCycleBuffer,
               driverPaymentCycleDuration = maybe config.driverPaymentCycleDuration (.value) req.driverPaymentCycleDuration,
               driverPaymentCycleStartTime = maybe config.driverPaymentCycleStartTime (.value) req.driverPaymentCycleStartTime,
               driverPaymentReminderInterval = maybe config.driverPaymentReminderInterval (.value) req.driverPaymentReminderInterval,
               timeDiffFromUtc = maybe config.timeDiffFromUtc (.value) req.timeDiffFromUtc,
               driverAutoPayNotificationTime = maybe config.driverAutoPayNotificationTime (.value) req.driverAutoPayNotificationTime,
               driverAutoPayExecutionTime = maybe config.driverAutoPayExecutionTime (.value) req.driverAutoPayExecutionTime,
               driverFeeMandateNotificationBatchSize = maybe config.driverFeeMandateNotificationBatchSize (.value) req.driverFeeMandateNotificationBatchSize,
               driverFeeMandateExecutionBatchSize = maybe config.driverFeeMandateExecutionBatchSize (.value) req.driverFeeMandateExecutionBatchSize,
               mandateNotificationRescheduleInterval = maybe config.mandateNotificationRescheduleInterval (.value) req.mandateNotificationRescheduleInterval,
               mandateExecutionRescheduleInterval = maybe config.mandateExecutionRescheduleInterval (.value) req.mandateExecutionRescheduleInterval,
               driverFeeCalculationTime = maybe config.driverFeeCalculationTime (.value) req.driverFeeCalculationTime,
               driverFeeCalculatorBatchSize = maybe config.driverFeeCalculatorBatchSize (.value) req.driverFeeCalculatorBatchSize,
               driverFeeCalculatorBatchGap = maybe config.driverFeeCalculatorBatchGap (.value) req.driverFeeCalculatorBatchGap,
               orderAndNotificationStatusCheckTime = fromMaybe config.orderAndNotificationStatusCheckTime (req.orderAndNotificationStatusCheckTime >>= (.value)),
               orderAndNotificationStatusCheckTimeLimit = fromMaybe config.orderAndNotificationStatusCheckTimeLimit (req.orderAndNotificationStatusCheckTimeLimit >>= (.value)),
               snapToRoadConfidenceThreshold = maybe config.snapToRoadConfidenceThreshold (.value) req.snapToRoadConfidenceThreshold,
               useWithSnapToRoadFallback = maybe config.useWithSnapToRoadFallback (.value) req.useWithSnapToRoadFallback
              }
  _ <- CQTC.update updConfig
  CQTC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigCommonUpdate : " (show merchant.id)
  pure Success

postMerchantSchedulerTrigger :: ShortId DM.Merchant -> Context.City -> Common.SchedulerTriggerReq -> Flow APISuccess
postMerchantSchedulerTrigger merchantShortId opCity req = do
  void $ findMerchantByShortId merchantShortId
  now <- getCurrentTime
  case req.scheduledAt of
    Just utcTime -> do
      let diffTimeS = diffUTCTime utcTime now
      triggerScheduler req.jobName req.jobData diffTimeS
    _ -> throwError $ InternalError "invalid scheduled at time"
  where
    triggerScheduler :: Maybe Common.JobName -> Text -> NominalDiffTime -> Flow APISuccess
    triggerScheduler jobName jobDataRaw diffTimeS = do
      case jobName of
        Just Common.DriverFeeCalculationTrigger -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe CalculateDriverFeesJobData
          case jobData' of
            Just jobData -> do
              let serviceName = fromMaybe Plan.YATRI_SUBSCRIPTION jobData.serviceName
                  mbMerchantOpCityId = jobData.merchantOperatingCityId
                  merchantId = jobData.merchantId
              merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
              merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
              when (serviceName == Plan.YATRI_RENTAL) $ do
                SDF.setCreateDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId True
              createJobIn @_ @'CalculateDriverFees (Just merchant.id) (Just merchantOpCityId) diffTimeS (jobData :: CalculateDriverFeesJobData)
              setDriverFeeCalcJobCache jobData.startTime jobData.endTime merchantOpCityId serviceName diffTimeS
              setDriverFeeBillNumberKey merchantOpCityId 1 36000 serviceName
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        Just Common.BadDebtCalculationTrigger -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe BadDebtCalculationJobData
          case jobData' of
            Just jobData -> do
              createJobIn @_ @'BadDebtCalculation (Just jobData.merchantId) (Just jobData.merchantOperatingCityId) diffTimeS (jobData :: BadDebtCalculationJobData)
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        Just Common.ReferralPayoutTrigger -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe DriverReferralPayoutJobData
          case jobData' of
            Just jobData -> do
              createJobIn @_ @'DriverReferralPayout (Just jobData.merchantId) (Just jobData.merchantOperatingCityId) diffTimeS (jobData :: DriverReferralPayoutJobData)
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        Just Common.SupplyDemandCalculation -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe SupplyDemandRequestJobData
          case jobData' of
            Just jobData -> do
              mbMerchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
              let mbMerchantOpCityId = mbMerchantOperatingCity <&> (.id)
              let mbMerchantId = mbMerchantOperatingCity <&> (.merchantId)
              createJobIn @_ @'SupplyDemand mbMerchantId mbMerchantOpCityId diffTimeS (jobData :: SupplyDemandRequestJobData)
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        _ -> throwError $ InternalError "invalid job name"

---------------------------------------------------------------------
getMerchantConfigDriverPool :: ShortId DM.Merchant -> Context.City -> Maybe Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Flow Common.DriverPoolConfigRes
getMerchantConfigDriverPool merchantShortId opCity reqTripDistance reqTripDistanceValue reqDistanceUnit = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let mbTripDistance =
        distanceToMeters <$> (Distance <$> reqTripDistanceValue <*> reqDistanceUnit)
          <|> reqTripDistance
  configs <- case mbTripDistance of
    Nothing -> CQDPC.findAllByMerchantOpCityId merchantOpCityId (Just []) Nothing
    Just tripDistance -> maybeToList <$> CQDPC.findByMerchantOpCityIdAndTripDistance merchantOpCityId tripDistance (Just []) Nothing
  pure $ mkDriverPoolConfigRes <$> configs

mkDriverPoolConfigRes :: DDPC.DriverPoolConfig -> Common.DriverPoolConfigItem
mkDriverPoolConfigRes DDPC.DriverPoolConfig {..} =
  Common.DriverPoolConfigItem
    { poolSortingType = castDPoolSortingType poolSortingType,
      minRadiusOfSearchWithUnit = convertMetersToDistance distanceUnit minRadiusOfSearch,
      maxRadiusOfSearchWithUnit = convertMetersToDistance distanceUnit maxRadiusOfSearch,
      radiusStepSizeWithUnit = convertMetersToDistance distanceUnit radiusStepSize,
      actualDistanceThresholdWithUnit = convertMetersToDistance distanceUnit <$> actualDistanceThreshold,
      tripDistanceWithUnit = convertMetersToDistance distanceUnit tripDistance,
      radiusShrinkValueForDriversOnRideWithUnit = convertMetersToDistance distanceUnit radiusShrinkValueForDriversOnRide,
      driverToDestinationDistanceThresholdWithUnit = convertMetersToDistance distanceUnit driverToDestinationDistanceThreshold,
      actualDistanceThresholdOnRideWithUnit = convertMetersToDistance distanceUnit <$> actualDistanceThresholdOnRide,
      ..
    }

castDPoolSortingType :: DriverPool.PoolSortingType -> Common.PoolSortingType
castDPoolSortingType = \case
  DriverPool.Intelligent -> Common.Intelligent
  DriverPool.Random -> Common.Random
  DriverPool.Tagged -> Common.Tagged

---------------------------------------------------------------------
postMerchantConfigDriverPoolUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Maybe Common.VehicleVariant ->
  Maybe Text ->
  Meters ->
  SL.Area ->
  Common.DriverPoolConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigDriverPoolUpdate merchantShortId opCity reqTripDistanceValue reqDistanceUnit mbVariant mbTripCategory reqTripDistance area req = do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let tripDistance = maybe reqTripDistance distanceToMeters (Distance <$> reqTripDistanceValue <*> reqDistanceUnit)
  let tripCategory = fromMaybe "All" mbTripCategory
  let serviceTier = DVeh.castVariantToServiceTier <$> mbVariant
  config <- CQDPC.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh merchantOpCityId tripDistance serviceTier tripCategory area (Just []) Nothing >>= fromMaybeM (DriverPoolConfigDoesNotExist merchantOpCityId.getId tripDistance)
  let updConfig =
        config{minRadiusOfSearch = mkDistanceField config.minRadiusOfSearch req.minRadiusOfSearchWithUnit req.minRadiusOfSearch,
               maxRadiusOfSearch = mkDistanceField config.maxRadiusOfSearch req.maxRadiusOfSearchWithUnit req.maxRadiusOfSearch,
               radiusStepSize = mkDistanceField config.radiusStepSize req.radiusStepSizeWithUnit req.radiusStepSize,
               driverPositionInfoExpiry = maybe config.driverPositionInfoExpiry (.value) req.driverPositionInfoExpiry,
               actualDistanceThreshold = mkOptionalDistanceField config.actualDistanceThreshold req.actualDistanceThresholdWithUnit req.actualDistanceThreshold,
               actualDistanceThresholdOnRide = mkOptionalDistanceField config.actualDistanceThresholdOnRide req.actualDistanceThresholdOnRideWithUnit req.actualDistanceThresholdOnRide,
               maxDriverQuotesRequired = maybe config.maxDriverQuotesRequired (.value) req.maxDriverQuotesRequired,
               driverQuoteLimit = maybe config.driverQuoteLimit (.value) req.driverQuoteLimit,
               driverRequestCountLimit = maybe config.driverRequestCountLimit (.value) req.driverRequestCountLimit,
               driverBatchSize = maybe config.driverBatchSize (.value) req.driverBatchSize,
               maxNumberOfBatches = maybe config.maxNumberOfBatches (.value) req.maxNumberOfBatches,
               maxParallelSearchRequests = maybe config.maxParallelSearchRequests (.value) req.maxParallelSearchRequests,
               maxParallelSearchRequestsOnRide = maybe config.maxParallelSearchRequestsOnRide (.value) req.maxParallelSearchRequestsOnRide,
               poolSortingType = maybe config.poolSortingType (castPoolSortingType . (.value)) req.poolSortingType,
               singleBatchProcessTime = maybe config.singleBatchProcessTime (.value) req.singleBatchProcessTime,
               distanceBasedBatchSplit = maybe config.distanceBasedBatchSplit (map castBatchSplitByPickupDistance . (.value)) req.distanceBasedBatchSplit
              }
  _ <- CQDPC.update updConfig
  CQDPC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigDriverPoolUpdate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

castPoolSortingType :: Common.PoolSortingType -> DriverPool.PoolSortingType
castPoolSortingType = \case
  Common.Intelligent -> DriverPool.Intelligent
  Common.Random -> DriverPool.Random
  Common.Tagged -> DriverPool.Tagged

castBatchSplitByPickupDistance :: Common.BatchSplitByPickupDistance -> DriverPool.BatchSplitByPickupDistance
castBatchSplitByPickupDistance Common.BatchSplitByPickupDistance {..} = DriverPool.BatchSplitByPickupDistance {..}

castOnRideSplitByPickupDistance :: Common.BatchSplitByPickupDistanceOnRide -> DriverPool.BatchSplitByPickupDistanceOnRide
castOnRideSplitByPickupDistance Common.BatchSplitByPickupDistanceOnRide {..} = DriverPool.BatchSplitByPickupDistanceOnRide {..}

castOnRideRadiusConfig :: Common.OnRideRadiusConfig -> DriverPool.OnRideRadiusConfig
castOnRideRadiusConfig Common.OnRideRadiusConfig {..} = DriverPool.OnRideRadiusConfig {onRideRadius = maybe onRideRadius distanceToMeters onRideRadiusWithUnit, ..}

---------------------------------------------------------------------
postMerchantConfigDriverPoolCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Maybe Common.VehicleVariant ->
  Maybe Text ->
  Meters ->
  SL.Area ->
  Common.DriverPoolConfigCreateReq ->
  Flow APISuccess
postMerchantConfigDriverPoolCreate merchantShortId opCity reqTripDistanceValue reqDistanceUnit mbVariant mbTripCategory reqTripDistance area req = do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just opCity)
  let (distanceUnit, merchantOpCityId) = (merchantOpCity.distanceUnit, merchantOpCity.id)
  let tripDistance = maybe reqTripDistance distanceToMeters (Distance <$> reqTripDistanceValue <*> reqDistanceUnit)
  let tripCategory = fromMaybe "All" mbTripCategory
  let serviceTier = DVeh.castVariantToServiceTier <$> mbVariant
  mbConfig <- CQDPC.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh merchantOpCityId tripDistance serviceTier tripCategory area (Just []) Nothing
  whenJust mbConfig $ \_ -> throwError (DriverPoolConfigAlreadyExists merchantOpCityId.getId tripDistance)
  newConfig <- buildDriverPoolConfig merchant.id merchantOpCityId tripDistance distanceUnit area serviceTier tripCategory req
  _ <- CQDPC.create newConfig
  -- We should clear cache here, because cache contains list of all configs for current merchantId
  CQDPC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigDriverPoolCreate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

buildDriverPoolConfig ::
  (MonadTime m, MonadGuid m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Meters ->
  DistanceUnit ->
  SL.Area ->
  Maybe ServiceTierType ->
  Text ->
  Common.DriverPoolConfigCreateReq ->
  m DDPC.DriverPoolConfig
buildDriverPoolConfig merchantId merchantOpCityId tripDistance distanceUnit area vehicleVariant tripCategory Common.DriverPoolConfigCreateReq {..} = do
  now <- getCurrentTime
  id <- generateGUID
  pure
    DDPC.DriverPoolConfig
      { merchantId,
        merchantOperatingCityId = merchantOpCityId,
        poolSortingType = castPoolSortingType poolSortingType,
        distanceBasedBatchSplit = map castBatchSplitByPickupDistance distanceBasedBatchSplit,
        scheduleTryTimes = [],
        updatedAt = now,
        createdAt = now,
        thresholdToIgnoreActualDistanceThreshold = Nothing,
        onRideBatchSplitConfig = map castOnRideSplitByPickupDistance onRideBatchSplitConfig,
        onRideRadiusConfig = map castOnRideRadiusConfig onRideRadiusConfig,
        batchSizeOnRide = batchSizeOnRide,
        timeBounds = Unbounded,
        minRadiusOfSearch = maybe minRadiusOfSearch distanceToMeters minRadiusOfSearchWithUnit,
        maxRadiusOfSearch = maybe maxRadiusOfSearch distanceToMeters maxRadiusOfSearchWithUnit,
        radiusStepSize = maybe radiusStepSize distanceToMeters radiusStepSizeWithUnit,
        actualDistanceThreshold = distanceToMeters <$> actualDistanceThresholdWithUnit <|> actualDistanceThreshold,
        radiusShrinkValueForDriversOnRide = maybe radiusShrinkValueForDriversOnRide distanceToMeters radiusShrinkValueForDriversOnRideWithUnit,
        driverToDestinationDistanceThreshold = maybe driverToDestinationDistanceThreshold distanceToMeters driverToDestinationDistanceThresholdWithUnit,
        actualDistanceThresholdOnRide = distanceToMeters <$> actualDistanceThresholdOnRideWithUnit <|> actualDistanceThresholdOnRide,
        ..
      }

---------------------------------------------------------------------
getMerchantConfigDriverIntelligentPool :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverIntelligentPoolConfigRes
getMerchantConfigDriverIntelligentPool merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CDIPC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (DriverIntelligentPoolConfigNotFound merchantOpCityId.getId)
  pure $ mkDriverIntelligentPoolConfigRes config

mkDriverIntelligentPoolConfigRes :: DDIPC.DriverIntelligentPoolConfig -> Common.DriverIntelligentPoolConfigRes
mkDriverIntelligentPoolConfigRes DDIPC.DriverIntelligentPoolConfig {..} = Common.DriverIntelligentPoolConfigRes {..}

---------------------------------------------------------------------
postMerchantConfigDriverIntelligentPoolUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigDriverIntelligentPoolUpdate merchantShortId opCity req = do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CDIPC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (DriverIntelligentPoolConfigNotFound merchantOpCityId.getId)
  let updConfig =
        config{availabilityTimeWeightage = maybe config.availabilityTimeWeightage (.value) req.availabilityTimeWeightage,
               availabilityTimeWindowOption = fromMaybe config.availabilityTimeWindowOption req.availabilityTimeWindowOption,
               acceptanceRatioWeightage = maybe config.acceptanceRatioWeightage (.value) req.acceptanceRatioWeightage,
               acceptanceRatioWindowOption = fromMaybe config.acceptanceRatioWindowOption req.acceptanceRatioWindowOption,
               cancellationRatioWeightage = maybe config.cancellationRatioWeightage (.value) req.cancellationRatioWeightage,
               cancellationAndRideFrequencyRatioWindowOption = fromMaybe config.cancellationAndRideFrequencyRatioWindowOption req.cancellationAndRideFrequencyRatioWindowOption,
               minQuotesToQualifyForIntelligentPool = maybe config.minQuotesToQualifyForIntelligentPool (.value) req.minQuotesToQualifyForIntelligentPool,
               minQuotesToQualifyForIntelligentPoolWindowOption = fromMaybe config.minQuotesToQualifyForIntelligentPoolWindowOption req.minQuotesToQualifyForIntelligentPoolWindowOption,
               intelligentPoolPercentage = maybe config.intelligentPoolPercentage (.value) req.intelligentPoolPercentage,
               speedNormalizer = maybe config.speedNormalizer (.value) req.speedNormalizer,
               driverSpeedWeightage = maybe config.driverSpeedWeightage (.value) req.driverSpeedWeightage,
               minLocationUpdates = maybe config.minLocationUpdates (.value) req.minLocationUpdates,
               locationUpdateSampleTime = maybe config.locationUpdateSampleTime (.value) req.locationUpdateSampleTime,
               defaultDriverSpeed = maybe config.defaultDriverSpeed (.value) req.defaultDriverSpeed
              }
  _ <- CQDIPC.update updConfig
  CQDIPC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigDriverIntelligentPoolUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
getMerchantConfigOnboardingDocument :: ShortId DM.Merchant -> Context.City -> Maybe Common.DocumentType -> Maybe Common.VehicleCategory -> Flow Common.DocumentVerificationConfigRes
getMerchantConfigOnboardingDocument merchantShortId opCity mbReqDocumentType mbCategory = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  configs <- case (mbReqDocumentType, mbCategory) of
    (Nothing, Nothing) -> CQDVC.findAllByMerchantOpCityId merchantOpCityId
    (Just reqDocumentType, Nothing) -> CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId (castDocumentType reqDocumentType)
    (Nothing, Just category) -> CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId category
    (Just reqDocumentType, Just category) -> maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId (castDocumentType reqDocumentType) category

  pure $ mkDocumentVerificationConfigRes <$> configs

mkDocumentVerificationConfigRes :: DVC.DocumentVerificationConfig -> Common.DocumentVerificationConfigItem
mkDocumentVerificationConfigRes DVC.DocumentVerificationConfig {..} =
  Common.DocumentVerificationConfigItem
    { documentType = castDDocumentType documentType,
      vehicleClassCheckType = castDVehicleClassCheckType vehicleClassCheckType,
      supportedVehicleClasses = castDSupportedVehicleClasses supportedVehicleClasses,
      rcNumberPrefixList = Just rcNumberPrefixList,
      ..
    }

castDSupportedVehicleClasses :: DVC.SupportedVehicleClasses -> Common.SupportedVehicleClasses
castDSupportedVehicleClasses = \case
  DVC.DLValidClasses cfg -> Common.DLValidClasses cfg
  DVC.RCValidClasses cfg -> Common.RCValidClasses (castDClassVariantMap <$> cfg)

castDClassVariantMap :: DVC.VehicleClassVariantMap -> Common.VehicleClassVariantMap
castDClassVariantMap DVC.VehicleClassVariantMap {..} =
  Common.VehicleClassVariantMap
    { vehicleVariant = vehicleVariant,
      vehicleModel = fromMaybe "" vehicleModel,
      ..
    }

castDVehicleClassCheckType :: DVC.VehicleClassCheckType -> Common.VehicleClassCheckType
castDVehicleClassCheckType = \case
  DVC.Infix -> Common.Infix
  DVC.Prefix -> Common.Prefix
  DVC.Suffix -> Common.Suffix

castDDocumentType :: DVC.DocumentType -> Common.DocumentType
castDDocumentType = \case
  DVC.VehicleRegistrationCertificate -> Common.RC
  DVC.DriverLicense -> Common.DL
  _ -> Common.RC -- fix later

---------------------------------------------------------------------
postMerchantConfigOnboardingDocumentUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DocumentType ->
  Common.VehicleCategory ->
  Common.DocumentVerificationConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentUpdate merchantShortId opCity reqDocumentType reqCategory req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType reqCategory >>= fromMaybeM (DocumentVerificationConfigDoesNotExist merchantOpCityId.getId $ show documentType)
  let updConfig =
        config{checkExtraction = maybe config.checkExtraction (.value) req.checkExtraction,
               checkExpiry = maybe config.checkExpiry (.value) req.checkExpiry,
               supportedVehicleClasses = maybe config.supportedVehicleClasses castSupportedVehicleClasses req.supportedVehicleClasses,
               vehicleClassCheckType = maybe config.vehicleClassCheckType (castVehicleClassCheckType . (.value)) req.vehicleClassCheckType,
               rcNumberPrefixList = maybe config.rcNumberPrefixList (.value) req.rcNumberPrefixList,
               maxRetryCount = maybe config.maxRetryCount (.value) req.maxRetryCount
              }
  _ <- CQDVC.update updConfig
  CQDVC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigOnboardingDocumentUpdate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

castSupportedVehicleClasses :: Common.SupportedVehicleClasses -> DVC.SupportedVehicleClasses
castSupportedVehicleClasses = \case
  Common.DLValidClasses cfg -> DVC.DLValidClasses cfg
  Common.RCValidClasses cfg -> DVC.RCValidClasses (castClassVariantMap <$> cfg)

castClassVariantMap :: Common.VehicleClassVariantMap -> DVC.VehicleClassVariantMap
castClassVariantMap Common.VehicleClassVariantMap {..} =
  DVC.VehicleClassVariantMap
    { vehicleVariant = vehicleVariant,
      vehicleModel = Just vehicleModel,
      ..
    }

castVehicleClassCheckType :: Common.VehicleClassCheckType -> DVC.VehicleClassCheckType
castVehicleClassCheckType = \case
  Common.Infix -> DVC.Infix
  Common.Prefix -> DVC.Prefix
  Common.Suffix -> DVC.Suffix

castDocumentType :: Common.DocumentType -> DVC.DocumentType
castDocumentType = \case
  Common.RC -> DVC.VehicleRegistrationCertificate
  Common.DL -> DVC.DriverLicense

---------------------------------------------------------------------
postMerchantConfigOnboardingDocumentCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DocumentType ->
  Common.VehicleCategory ->
  Common.DocumentVerificationConfigCreateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentCreate merchantShortId opCity reqDocumentType reqCategory req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let documentType = castDocumentType reqDocumentType
  mbConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType reqCategory
  whenJust mbConfig $ \_ -> throwError (DocumentVerificationConfigAlreadyExists merchantOpCityId.getId $ show documentType)
  newConfig <- buildDocumentVerificationConfig merchant.id merchantOpCityId documentType req
  _ <- CQDVC.create newConfig
  -- We should clear cache here, because cache contains list of all configs for current merchantId
  CQDVC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantConfigOnboardingDocumentCreate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

buildDocumentVerificationConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DVC.DocumentType ->
  Common.DocumentVerificationConfigCreateReq ->
  m DVC.DocumentVerificationConfig
buildDocumentVerificationConfig merchantId merchantOpCityId documentType Common.DocumentVerificationConfigCreateReq {..} = do
  now <- getCurrentTime
  pure
    DVC.DocumentVerificationConfig
      { merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        vehicleClassCheckType = castVehicleClassCheckType vehicleClassCheckType,
        supportedVehicleClasses = castSupportedVehicleClasses supportedVehicleClasses,
        rcNumberPrefixList = fromMaybe [] rcNumberPrefixList,
        dependencyDocumentType = [],
        description = Nothing,
        disableWarning = Nothing,
        isDisabled = False,
        isHidden = False,
        isMandatory = False,
        title = "Empty title",
        vehicleCategory = DVC.AUTO_CATEGORY,
        order = 0,
        isDefaultEnabledOnManualVerification = fromMaybe True isDefaultEnabledOnManualVerification,
        isImageValidationRequired = fromMaybe True isImageValidationRequired,
        doStrictVerifcation = fromMaybe True doStrictVerifcation,
        updatedAt = now,
        createdAt = now,
        ..
      }

---------------------------------------------------------------------
postMerchantServiceConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigMapsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig merchanOperatingCityId
  CQMSC.upsertMerchantServiceConfig merchantServiceConfig merchanOperatingCityId
  CQMSC.clearCache serviceName merchanOperatingCityId
  logTagInfo "dashboard -> postMerchantServiceConfigMapsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigSmsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig merchanOperatingCityId
  CQMSC.upsertMerchantServiceConfig merchantServiceConfig merchanOperatingCityId
  CQMSC.clearCache serviceName merchanOperatingCityId
  logTagInfo "dashboard -> postMerchantServiceConfigSmsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
getMerchantServiceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.ServiceUsageConfigRes
getMerchantServiceUsageConfig merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  pure $ mkServiceUsageConfigRes config

mkServiceUsageConfigRes :: DMSUC.MerchantServiceUsageConfig -> Common.ServiceUsageConfigRes
mkServiceUsageConfigRes DMSUC.MerchantServiceUsageConfig {..} =
  Common.ServiceUsageConfigRes
    { getEstimatedPickupDistances = Just getEstimatedPickupDistances,
      getPickupRoutes = Just getPickupRoutes,
      getTripRoutes = Just getTripRoutes,
      ..
    }

---------------------------------------------------------------------
postMerchantServiceUsageConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByServiceAndCity (DMSC.MapsService service) merchantOpCityId
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getEstimatedPickupDistances = fromMaybe merchantServiceUsageConfig.getEstimatedPickupDistances req.getEstimatedPickupDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOpCityId
  logTagInfo "dashboard -> mapsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByServiceAndCity (DMSC.SmsService service) merchantOpCityId
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOpCityId
  logTagInfo "dashboard -> postMerchantServiceUsageConfigSmsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceConfigVerificationUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.VerificationServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigVerificationUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  let serviceName = DMSC.VerificationService $ Common.getVerificationServiceFromReq req
  serviceConfig <- DMSC.VerificationServiceConfig <$> Common.buildVerificationServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig merchanOperatingCityId
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig merchanOperatingCityId
  CQMSC.clearCache serviceName merchanOperatingCityId
  logTagInfo "dashboard -> postMerchantServiceConfigVerificationUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantShortId city reqFarePolicyId reqStartDistanceValue reqDistanceUnit reqStartDistance req = do
  let farePolicyId = cast reqFarePolicyId
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.getMerchantOpCity merchant (Just city)
  let startDistance = maybe reqStartDistance distanceToMeters (Distance <$> reqStartDistanceValue <*> reqDistanceUnit)
  SMerchant.checkCurrencies merchantOperatingCity.currency [req.minFeeWithCurrency, req.maxFeeWithCurrency, req.stepFeeWithCurrency, req.defaultStepFeeWithCurrency]
  mbFarePolicy <- QFPEFB.findByFarePolicyIdAndStartDistance farePolicyId startDistance
  whenJust mbFarePolicy $ \_ -> throwError $ InvalidRequest "Fare policy with the same id and startDistance already exists"
  farePolicyDetails <- buildFarePolicy farePolicyId startDistance req merchantOperatingCity.distanceUnit
  _ <- QFPEFB.create farePolicyDetails
  CQFP.clearCacheById farePolicyId
  pure Success
  where
    buildFarePolicy fpId strtDistance request distanceUnit = do
      let driverExtraFeeBounds =
            DFPEFB.DriverExtraFeeBounds
              { startDistance = strtDistance,
                minFee = fromMaybe (toHighPrecMoney request.minFee) $ request.minFeeWithCurrency <&> (.amount),
                maxFee = fromMaybe (toHighPrecMoney request.maxFee) $ request.maxFeeWithCurrency <&> (.amount),
                stepFee = fromMaybe (toHighPrecMoney request.stepFee) $ request.stepFeeWithCurrency <&> (.amount),
                defaultStepFee = fromMaybe (toHighPrecMoney request.defaultStepFee) $ request.defaultStepFeeWithCurrency <&> (.amount),
                distanceUnit
              }
      return (fpId, driverExtraFeeBounds)

---------------------------------------------------------------------
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantShortId city reqFarePolicyId reqStartDistanceValue reqDistanceUnit reqStartDistance req = do
  let farePolicyId = cast reqFarePolicyId
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.getMerchantOpCity merchant (Just city)
  let startDistance = maybe reqStartDistance distanceToMeters (Distance <$> reqStartDistanceValue <*> reqDistanceUnit)
  SMerchant.checkCurrencies merchantOperatingCity.currency [req.minFeeWithCurrency, req.maxFeeWithCurrency, req.stepFeeWithCurrency, req.defaultStepFeeWithCurrency]
  let reqMinFee = fromMaybe (toHighPrecMoney req.minFee) $ req.minFeeWithCurrency <&> (.amount)
  let reqMaxFee = fromMaybe (toHighPrecMoney req.maxFee) $ req.maxFeeWithCurrency <&> (.amount)
  _ <- QFPEFB.findByFarePolicyIdAndStartDistance farePolicyId startDistance >>= fromMaybeM (InvalidRequest "Fare Policy with given id and startDistance not found")
  _ <- QFPEFB.update farePolicyId startDistance reqMinFee reqMaxFee
  CQFP.clearCacheById farePolicyId
  pure Success

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Meters -> Common.UpdateFPPerExtraKmRateReq -> Flow APISuccess
postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantShortId city reqFarePolicyId startDistance req = do
  let farePolicyId = cast reqFarePolicyId
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.getMerchantOpCity merchant (Just city)
  SMerchant.checkCurrencies merchantOperatingCity.currency [req.perExtraKmRateWithCurrency]
  _ <- QFPPDEKM.findByIdAndStartDistance farePolicyId startDistance >>= fromMaybeM (InvalidRequest "Fare Policy Parameters Per Extra Km Section with given id and start distance not found")
  _ <- QFPPDEKM.updatePerExtraKmRate farePolicyId startDistance $ fromMaybe req.perExtraKmRate (req.perExtraKmRateWithCurrency <&> (.amount))
  CQFP.clearCacheById farePolicyId
  pure Success

postMerchantConfigFarePolicyUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Common.UpdateFarePolicyReq -> Flow APISuccess
postMerchantConfigFarePolicyUpdate _ _ reqFarePolicyId req = do
  let farePolicyId = cast reqFarePolicyId
  farePolicy <- CQFP.findById Nothing farePolicyId >>= fromMaybeM (InvalidRequest "Fare Policy with given id not found")
  SMerchant.checkCurrencies farePolicy.currency $
    [ req.serviceChargeWithCurrency,
      req.perMinuteRideExtraTimeChargeWithCurrency,
      req.baseFareWithCurrency,
      req.deadKmFareWithCurrency
    ]
      <> maybe [] FarePolicy.getWaitingChargeFields req.waitingCharge
      <> maybe [] FarePolicy.getWaitingChargeInfoFields req.waitingChargeInfo
      <> maybe [] FarePolicy.getNightShiftChargeFields req.nightShiftCharge
  updatedFarePolicy <- mkUpdatedFarePolicy farePolicy
  CQFP.update' updatedFarePolicy
  CQFP.clearCacheById farePolicyId
  pure Success
  where
    mkUpdatedFarePolicy FarePolicy.FarePolicy {..} = do
      fPDetails <- mkFarePolicyDetails id farePolicyDetails
      pure $
        FarePolicy.FarePolicy
          { serviceCharge = (req.serviceChargeWithCurrency <&> (.amount)) <|> (toHighPrecMoney <$> req.serviceCharge) <|> serviceCharge,
            nightShiftBounds = req.nightShiftBounds <|> nightShiftBounds,
            allowedTripDistanceBounds = (FarePolicy.mkAllowedTripDistanceBounds distanceUnit <$> req.allowedTripDistanceBounds) <|> allowedTripDistanceBounds,
            govtCharges = req.govtCharges <|> govtCharges,
            perMinuteRideExtraTimeCharge = (req.perMinuteRideExtraTimeChargeWithCurrency <&> (.amount)) <|> req.perMinuteRideExtraTimeCharge <|> perMinuteRideExtraTimeCharge,
            tollCharges = req.tollCharges <|> tollCharges,
            farePolicyDetails = fPDetails,
            congestionChargeMultiplier = FarePolicy.mkCongestionChargeMultiplier <$> req.congestionChargeMultiplier <|> congestionChargeMultiplier,
            description = req.description <|> description,
            ..
          }

    mkFarePolicyDetails farePolicyId fPDetails =
      case fPDetails of
        FarePolicy.ProgressiveDetails _ -> do
          (_, fPProgressiveDetails) <- QFPPD.findById' farePolicyId >>= fromMaybeM (InvalidRequest "Fare Policy Progressive Details not found")
          pure $ FarePolicy.ProgressiveDetails $ mkUpdatedFPProgressiveDetails fPProgressiveDetails
        FarePolicy.SlabsDetails _ -> pure fPDetails
        FarePolicy.RentalDetails _ -> pure fPDetails
        FarePolicy.InterCityDetails _ -> pure fPDetails
        FarePolicy.AmbulanceDetails _ -> pure fPDetails

    mkUpdatedFPProgressiveDetails FarePolicy.FPProgressiveDetails {..} = do
      FarePolicy.FPProgressiveDetails
        { baseFare = fromMaybe baseFare $ (req.baseFareWithCurrency <&> (.amount)) <|> (toHighPrecMoney <$> req.baseFare),
          baseDistance = fromMaybe baseDistance $ distanceToMeters <$> req.baseDistanceWithUnit <|> req.baseDistance,
          deadKmFare = fromMaybe deadKmFare $ (req.deadKmFareWithCurrency <&> (.amount)) <|> (toHighPrecMoney <$> req.deadKmFare),
          waitingChargeInfo = FarePolicy.mkWaitingChargeInfo <$> req.waitingChargeInfo <|> waitingChargeInfo,
          nightShiftCharge = FarePolicy.mkNightShiftCharge <$> req.nightShiftCharge <|> nightShiftCharge,
          ..
        }

---------------------------------------------------------------------
data FarePolicyCSVRow = FarePolicyCSVRow
  { city :: Text,
    vehicleServiceTier :: Text,
    area :: Text,
    tripCategory :: Text,
    farePolicyKey :: Text,
    nightShiftStart :: Text,
    nightShiftEnd :: Text,
    minAllowedTripDistance :: Text,
    maxAllowedTripDistance :: Text,
    serviceCharge :: Text,
    tollCharges :: Text,
    tipOptions :: Text,
    govtCharges :: Text,
    farePolicyType :: Text,
    description :: Text,
    congestionChargeMultiplier :: Text,
    congestionChargeMultiplierIncludeBaseFare :: Text,
    parkingCharge :: Text,
    perStopCharge :: Text,
    currency :: Text,
    baseDistance :: Text,
    baseFare :: Text,
    deadKmFare :: Text,
    waitingCharge :: Text,
    waitingChargeType :: Text,
    nightShiftCharge :: Text,
    nightShiftChargeType :: Text,
    freeWatingTime :: Text,
    startDistanceDriverAddition :: Text,
    minFee :: Text,
    maxFee :: Text,
    stepFee :: Text,
    defaultStepFee :: Text,
    extraKmRateStartDistance :: Text,
    perExtraKmRate :: Text,
    baseFareDepreciation :: Text,
    peakTimings :: Text,
    peakDays :: Text,
    cancellationFarePolicyDescription :: Text,
    freeCancellationTimeSeconds :: Text,
    maxCancellationCharge :: Text,
    maxWaitingTimeAtPickupSeconds :: Text,
    minCancellationCharge :: Text,
    perMetreCancellationCharge :: Text,
    perMinuteCancellationCharge :: Text,
    percentageOfRideFareToBeCharged :: Text,
    platformFeeChargeType :: Text,
    platformFeeCharge :: Text,
    platformFeeCgst :: Text,
    platformFeeSgst :: Text,
    platformFeeChargeFarePolicyLevel :: Text,
    platformFeeCgstFarePolicyLevel :: Text,
    platformFeeSgstFarePolicyLevel :: Text,
    platformFeeChargesBy :: Text,
    perMinuteRideExtraTimeCharge :: Text,
    searchSource :: Text,
    perExtraMinRate :: Text,
    includedKmPerHr :: Text,
    plannedPerKmRate :: Text,
    maxAdditionalKmsLimit :: Text,
    totalAdditionalKmsLimit :: Text,
    timePercentage :: Text,
    distancePercentage :: Text,
    farePercentage :: Text,
    includeActualTimePercentage :: Text,
    includeActualDistPercentage :: Text,
    rideDuration :: Text,
    bufferKms :: Text,
    bufferMeters :: Text,
    perHourCharge :: Text,
    perKmRateOneWay :: Text,
    perKmRateRoundTrip :: Text,
    kmPerPlannedExtraHour :: Text,
    perDayMaxHourAllowance :: Text,
    perDayMaxAllowanceInMins :: Text,
    defaultWaitTimeAtDestination :: Text,
    enabled :: Text,
    disableRecompute :: Text,
    stateEntryPermitCharges :: Text
  }
  deriving (Show)

instance FromNamedRecord FarePolicyCSVRow where
  parseNamedRecord r =
    FarePolicyCSVRow
      <$> r .: "city"
      <*> r .: "vehicle_service_tier"
      <*> r .: "area"
      <*> r .: "tripCategory"
      <*> r .: "fare_policy_key"
      <*> r .: "night_shift_start"
      <*> r .: "night_shift_end"
      <*> r .: "min_allowed_trip_distance"
      <*> r .: "max_allowed_trip_distance"
      <*> r .: "service_charge"
      <*> r .: "toll_charges"
      <*> r .: "tip_options"
      <*> r .: "govt_charges"
      <*> r .: "fare_policy_type"
      <*> r .: "description"
      <*> r .: "congestion_charge_multiplier"
      <*> r .: "congestion_charge_multiplier_include_base_fare"
      <*> r .: "parking_charge"
      <*> r .: "per_stop_charge"
      <*> r .: "currency"
      <*> r .: "base_distance"
      <*> r .: "base_fare"
      <*> r .: "dead_km_fare"
      <*> r .: "waiting_charge"
      <*> r .: "waiting_charge_type"
      <*> r .: "night_shift_charge"
      <*> r .: "night_shift_charge_type"
      <*> r .: "free_wating_time"
      <*> r .: "start_distance_driver_addition"
      <*> r .: "min_fee"
      <*> r .: "max_fee"
      <*> r .: "step_fee"
      <*> r .: "default_step_fee"
      <*> r .: "extra_km_rate_start_distance"
      <*> r .: "per_extra_km_rate"
      <*> r .: "base_fare_depreciation"
      <*> r .: "peak_timings"
      <*> r .: "peak_days"
      <*> r .: "cancellation_fare_policy_description"
      <*> r .: "free_cancellation_time_seconds"
      <*> r .: "max_cancellation_charge"
      <*> r .: "max_waiting_time_at_pickup_seconds"
      <*> r .: "min_cancellation_charge"
      <*> r .: "per_metre_cancellation_charge"
      <*> r .: "per_minute_cancellation_charge"
      <*> r .: "percentage_of_ride_fare_to_be_charged"
      <*> r .: "platform_fee_charge_type"
      <*> r .: "platform_fee_charge"
      <*> r .: "platform_fee_cgst"
      <*> r .: "platform_fee_sgst"
      <*> r .: "platform_fee_charge_fare_policy_level"
      <*> r .: "platform_fee_cgst_fare_policy_level"
      <*> r .: "platform_fee_sgst_pare_policy_level"
      <*> r .: "platform_fee_charges_by"
      <*> r .: "per_minute_ride_extra_time_charge"
      <*> r .: "search_source"
      <*> r .: "per_extra_min_rate"
      <*> r .: "included_km_per_hr"
      <*> r .: "planned_per_km_rate"
      <*> r .: "max_additional_kms_limit"
      <*> r .: "total_additional_kms_limit"
      <*> r .: "time_percentage"
      <*> r .: "distance_percentage"
      <*> r .: "fare_percentage"
      <*> r .: "include_actual_time_percentage"
      <*> r .: "include_actual_dist_percentage"
      <*> r .: "ride_duration"
      <*> r .: "buffer_kms"
      <*> r .: "buffer_meters"
      <*> r .: "per_hour_charge"
      <*> r .: "per_km_rate_one_way"
      <*> r .: "per_km_rate_round_trip"
      <*> r .: "km_per_planned_extra_hour"
      <*> r .: "per_day_max_hour_allowance"
      <*> r .: "per_day_max_allowance_in_mins"
      <*> r .: "default_wait_time_at_destination"
      <*> r .: "enabled"
      <*> r .: "disable_recompute"
      <*> r .: "state_entry_permit_charges"

merchantCityLockKey :: Text -> Text
merchantCityLockKey id = "Driver:MerchantOperating:CityId-" <> id

postMerchantConfigFarePolicyUpsert :: ShortId DM.Merchant -> Context.City -> Common.UpsertFarePolicyReq -> Flow Common.UpsertFarePolicyResp
postMerchantConfigFarePolicyUpsert merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  result <-
    Hedis.whenWithLockRedisAndReturnValue (merchantCityLockKey merchantOpCity.id.getId) 60 $ do
      logTagInfo "Updating Fare Policies for merchant: " (show merchant.id <> " and city: " <> show opCity)
      flatFarePolicies <- readCsv merchant.id merchantOpCity.distanceUnit req.file merchantOpCity.id
      logTagInfo "Read file: " (show flatFarePolicies)
      let boundedAlreadyDeletedMap = Map.empty :: Map.Map Text Bool
      (farePolicyErrors, _) <- (foldlM (processFarePolicyGroup merchantOpCity) ([], boundedAlreadyDeletedMap) . groupFarePolices) flatFarePolicies
      return $
        Common.UpsertFarePolicyResp
          { unprocessedFarePolicies = farePolicyErrors,
            success = "Fare Policies updated successfully"
          }
  case result of
    Right res -> return res
    Left _ -> throwError $ InvalidRequest "Someone already triggered this api"
  where
    cleanField = replaceEmpty . T.strip

    readCsv merchantId distanceUnit csvFile merchantOpCity = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector FarePolicyCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM (makeFarePolicy merchantId merchantOpCity distanceUnit) v >>= (pure . V.toList)

    readCSVField :: Read a => Int -> Text -> Text -> Flow a
    readCSVField idx fieldValue fieldName =
      cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
    readMaybeCSVField _ fieldValue _ =
      cleanField fieldValue >>= readMaybe . T.unpack

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    cleanMaybeCSVField :: Int -> Text -> Text -> Maybe Text
    cleanMaybeCSVField _ fieldValue _ = cleanField fieldValue

    groupFarePolices :: [(Maybe Bool, Context.City, ServiceTierType, TripCategory, SL.Area, TimeBound, DFareProduct.SearchSource, Bool, FarePolicy.FarePolicy)] -> [[(Maybe Bool, Context.City, ServiceTierType, TripCategory, SL.Area, TimeBound, DFareProduct.SearchSource, Bool, FarePolicy.FarePolicy)]]
    groupFarePolices = DL.groupBy (\a b -> fst7 a == fst7 b) . DL.sortBy (compare `on` fst7)
      where
        fst7 (dr, c, t, tr, a, tb, ss, en, _) = (dr, c, t, tr, a, tb, ss, en)

    processFarePolicyGroup :: DMOC.MerchantOperatingCity -> ([Text], Map.Map Text Bool) -> [(Maybe Bool, Context.City, ServiceTierType, TripCategory, SL.Area, TimeBound, DFareProduct.SearchSource, Bool, FarePolicy.FarePolicy)] -> Flow ([Text], Map.Map Text Bool)
    processFarePolicyGroup _ _ [] = throwError $ InvalidRequest "Empty Fare Policy Group"
    processFarePolicyGroup merchantOpCity (errors, boundedAlreadyDeletedMap) (x : xs) = do
      let (disableRecompute, city, vehicleServiceTier, tripCategory, area, timeBounds, searchSource, enabled', firstFarePolicy) = x
      if city /= opCity
        then return $ (errors <> ["Can't process fare policy for different city: " <> show city <> ", please login with this city in dashboard"], boundedAlreadyDeletedMap)
        else do
          let mergeFarePolicy newId FarePolicy.FarePolicy {..} = do
                let remainingfarePolicies = map (\(_, _, _, _, _, _, _, _, fp) -> fp) xs
                let driverExtraFeeBounds' = NE.nonEmpty $ maybe [] NE.toList driverExtraFeeBounds <> concatMap (maybe [] NE.toList . (.driverExtraFeeBounds)) remainingfarePolicies
                let driverExtraFeeBoundsDuplicateRemoved = NE.nubBy (\a b -> a.startDistance == b.startDistance) <$> driverExtraFeeBounds'
                farePolicyDetails' <-
                  case farePolicyDetails of
                    FarePolicy.ProgressiveDetails FarePolicy.FPProgressiveDetails {currency = _currency', distanceUnit = _distanceUnit', ..} -> do
                      remainingPerKmSections <-
                        mapM
                          ( \f ->
                              case f.farePolicyDetails of
                                FarePolicy.ProgressiveDetails details -> return $ NE.toList details.perExtraKmRateSections
                                _ -> throwError $ InvalidRequest "Please have same fare policy type for all fare policies of a area, service tier, trip category and time bound"
                          )
                          remainingfarePolicies
                      let perExtraKmRateSections' =
                            case remainingPerKmSections of
                              [] -> perExtraKmRateSections
                              _ -> perExtraKmRateSections <> NE.fromList (concat remainingPerKmSections)
                      let perExtraKmRateSectionsDuplicateRemoved = NE.nubBy (\a b -> a.startDistance == b.startDistance) perExtraKmRateSections'
                      return $ FarePolicy.ProgressiveDetails FarePolicy.FPProgressiveDetails {perExtraKmRateSections = perExtraKmRateSectionsDuplicateRemoved, ..}
                    FarePolicy.SlabsDetails FarePolicy.FPSlabsDetails {..} -> do
                      remainingSlabs <-
                        mapM
                          ( \f ->
                              case f.farePolicyDetails of
                                FarePolicy.SlabsDetails details -> return $ NE.toList details.slabs
                                _ -> throwError $ InvalidRequest "Please have same fare policy type for all fare policies of a area, service tier, trip category and time bound"
                          )
                          remainingfarePolicies
                      let slabs' =
                            case remainingSlabs of
                              [] -> slabs
                              _ -> slabs <> NE.fromList (concat remainingSlabs)
                      let slabsDuplicateRemoved = NE.nubBy (\a b -> a.startDistance == b.startDistance) slabs'
                      return $ FarePolicy.SlabsDetails FarePolicy.FPSlabsDetails {slabs = slabsDuplicateRemoved}
                    FarePolicy.RentalDetails FarePolicy.FPRentalDetails {currency = _currency', ..} -> do
                      remainingRentalDistanceBuffer <-
                        mapM
                          ( \f ->
                              case f.farePolicyDetails of
                                FarePolicy.RentalDetails details -> return $ NE.toList details.distanceBuffers
                                _ -> throwError $ InvalidRequest "Please have same fare policy type for all fare policies of a area, service tier, trip category and time bound"
                          )
                          remainingfarePolicies
                      remainingPricingSlabs <-
                        mapM
                          ( \f ->
                              case f.farePolicyDetails of
                                FarePolicy.RentalDetails details -> return $ NE.toList details.pricingSlabs
                                _ -> throwError $ InvalidRequest "Please have same fare policy type for all fare policies of a area, service tier, trip category and time bound"
                          )
                          remainingfarePolicies
                      let pricingSlabs' =
                            case remainingPricingSlabs of
                              [] -> pricingSlabs
                              _ -> pricingSlabs <> NE.fromList (concat remainingPricingSlabs)
                      let distanceBuffers' =
                            case remainingRentalDistanceBuffer of
                              [] -> distanceBuffers
                              _ -> distanceBuffers <> NE.fromList (concat remainingRentalDistanceBuffer)
                      let distanceBuffersDuplicateRemoved = NE.nubBy (\a b -> a.rideDuration == b.rideDuration) distanceBuffers'
                      let pricingSlabsDuplicateRemoved = NE.nubBy (\a b -> a.timePercentage == b.timePercentage && a.distancePercentage == b.distancePercentage) pricingSlabs'
                      return $ FarePolicy.RentalDetails FarePolicy.FPRentalDetails {distanceBuffers = distanceBuffersDuplicateRemoved, pricingSlabs = pricingSlabsDuplicateRemoved, ..}
                    FarePolicy.InterCityDetails FarePolicy.FPInterCityDetails {currency = _currency', ..} -> do
                      remainingPricingSlabs <-
                        mapM
                          ( \f ->
                              case f.farePolicyDetails of
                                FarePolicy.InterCityDetails details -> return $ NE.toList details.pricingSlabs
                                _ -> throwError $ InvalidRequest "Please have same fare policy type for all fare policies of a area, service tier, trip category and time bound"
                          )
                          remainingfarePolicies
                      let pricingSlabs' =
                            case remainingPricingSlabs of
                              [] -> pricingSlabs
                              _ -> pricingSlabs <> NE.fromList (concat remainingPricingSlabs)

                      let pricingSlabsDuplicateRemoved = NE.nubBy (\a b -> a.timePercentage == b.timePercentage && a.distancePercentage == b.distancePercentage) pricingSlabs'
                      return $ FarePolicy.InterCityDetails FarePolicy.FPInterCityDetails {pricingSlabs = pricingSlabsDuplicateRemoved, ..}
                    _ -> return farePolicyDetails
                return $ FarePolicy.FarePolicy {id = newId, driverExtraFeeBounds = driverExtraFeeBoundsDuplicateRemoved, farePolicyDetails = farePolicyDetails', ..}

          newId <- generateGUID
          finalFarePolicy <- mergeFarePolicy newId firstFarePolicy
          CQFP.create finalFarePolicy
          let merchanOperatingCityId = merchantOpCity.id
          (oldFareProducts, newBoundedAlreadyDeletedMap) <-
            case timeBounds of
              Unbounded -> do
                fareProducts <- SQF.findAllUnboundedByMerchantOpCityIdVariantArea merchanOperatingCityId area tripCategory vehicleServiceTier TB.Unbounded True [searchSource]
                return (fareProducts, boundedAlreadyDeletedMap)
              _ -> do
                let key = makeKey merchanOperatingCityId vehicleServiceTier tripCategory area searchSource
                let value = Map.lookup key boundedAlreadyDeletedMap
                if isJust value
                  then return ([], boundedAlreadyDeletedMap)
                  else do
                    fareProducts <- CQFProduct.findAllBoundedByMerchantVariantArea merchanOperatingCityId [searchSource] tripCategory vehicleServiceTier area
                    let updatedBoundedAlreadyDeletedMap = markBoundedAreadyDeleted merchanOperatingCityId vehicleServiceTier tripCategory area searchSource boundedAlreadyDeletedMap
                    return (fareProducts, updatedBoundedAlreadyDeletedMap)

          oldFareProducts `forM_` \fp -> do
            fareProducts <- CQFProduct.findAllFareProductByFarePolicyId fp.farePolicyId
            when (length fareProducts == 1) $ CQFP.delete fp.farePolicyId
            CQFProduct.delete fp.id
            CQFProduct.clearCache fp

          id <- generateGUID
          let farePolicyId = finalFarePolicy.id
          let fareProduct = DFareProduct.FareProduct {enabled = enabled', merchantId = merchantOpCity.merchantId, merchantOperatingCityId = merchantOpCity.id, ..}
          CQFProduct.create fareProduct
          CQFProduct.clearCache fareProduct
          oldFareProducts `forM_` CQFProduct.clearCache

          return (errors, newBoundedAlreadyDeletedMap)

    checkIfvehicleServiceTierExists vehicleServiceTier merchanOperatingCityId = CQVST.findByServiceTierTypeAndCityId merchanOperatingCityId vehicleServiceTier >>= fromMaybeM (VehicleServiceTierNotFound $ show vehicleServiceTier)

    makeFarePolicy :: Id DM.Merchant -> Id MerchantOperatingCity -> DistanceUnit -> Int -> FarePolicyCSVRow -> Flow (Maybe Bool, Context.City, ServiceTierType, TripCategory, SL.Area, TimeBound, DFareProduct.SearchSource, Bool, FarePolicy.FarePolicy)
    makeFarePolicy merchantId merchantOpCity distanceUnit idx row = do
      now <- getCurrentTime
      let createdAt = now
      let updatedAt = now
      let mbPeakTimings = cleanField row.peakTimings
      timeBound <-
        case mbPeakTimings of
          Nothing -> return Unbounded
          _ -> do
            peakTimings :: [(TimeOfDay, TimeOfDay)] <- readCSVField idx row.peakTimings "Peak Timings"
            peakDays :: [DayOfWeek] <- readCSVField idx row.peakDays "Peak Days"
            let bounds =
                  BoundedPeaks
                    { monday = if Monday `elem` peakDays then peakTimings else [],
                      tuesday = if Tuesday `elem` peakDays then peakTimings else [],
                      wednesday = if Wednesday `elem` peakDays then peakTimings else [],
                      thursday = if Thursday `elem` peakDays then peakTimings else [],
                      friday = if Friday `elem` peakDays then peakTimings else [],
                      saturday = if Saturday `elem` peakDays then peakTimings else [],
                      sunday = if Sunday `elem` peakDays then peakTimings else []
                    }
            return $ BoundedByWeekday bounds
      city :: Context.City <- readCSVField idx row.city "City"
      vehicleServiceTier :: ServiceTierType <- readCSVField idx row.vehicleServiceTier "Vehicle Service Tier"
      _ <- checkIfvehicleServiceTierExists merchantOpCity vehicleServiceTier
      area :: SL.Area <- readCSVField idx row.area "Area"
      idText <- cleanCSVField idx row.farePolicyKey "Fare Policy Key"
      tripCategory :: TripCategory <- readCSVField idx row.tripCategory "Trip Category"
      enabled :: Bool <- readCSVField idx row.enabled "Enabled"
      let searchSource :: DFareProduct.SearchSource = fromMaybe DFareProduct.ALL $ readMaybeCSVField idx row.searchSource "Search Source"
      nightShiftStart :: TimeOfDay <- readCSVField idx row.nightShiftStart "Night Shift Start"
      nightShiftEnd :: TimeOfDay <- readCSVField idx row.nightShiftEnd "Night Shift End"
      let nightShiftBounds = Just $ Common.NightShiftBounds nightShiftStart nightShiftEnd
      minAllowedTripDistance :: Meters <- readCSVField idx row.minAllowedTripDistance "Min Allowed Trip Distance"
      maxAllowedTripDistance :: Meters <- readCSVField idx row.maxAllowedTripDistance "Max Allowed Trip Distance"
      let allowedTripDistanceBounds = Just $ FarePolicy.AllowedTripDistanceBounds {distanceUnit, minAllowedTripDistance, maxAllowedTripDistance}
      let serviceCharge :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.serviceCharge "Service Charge"
      let tollCharges :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.tollCharges "Toll Charge"
      let tipOptions :: (Maybe [Int]) = readMaybeCSVField idx row.tipOptions "Tip Options"
      let perMinuteRideExtraTimeCharge :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.perMinuteRideExtraTimeCharge "Per Minute Ride Extra Time Charge"
      let govtCharges :: (Maybe Double) = readMaybeCSVField idx row.govtCharges "Govt Charges"
      farePolicyType :: FarePolicy.FarePolicyType <- readCSVField idx row.farePolicyType "Fare Policy Type"
      void $ validateFarePolicyType farePolicyType tripCategory
      let platformFeeChargeFarePolicyLevel :: Maybe HighPrecMoney = readMaybeCSVField idx row.platformFeeChargeFarePolicyLevel "Platform Fee Charge"
      let platformFeeCgstFarePolicyLevel :: Maybe HighPrecMoney = readMaybeCSVField idx row.platformFeeCgstFarePolicyLevel "Platform Fee CGST Amount"
      let platformFeeSgstFarePolicyLevel :: Maybe HighPrecMoney = readMaybeCSVField idx row.platformFeeSgstFarePolicyLevel "Platform Fee SGST Amount"
      let platformFeeChargesBy :: Maybe FarePolicy.PlatformFeeMethods = readMaybeCSVField idx row.platformFeeChargesBy "Platform Fee Charges By"
      description <- cleanCSVField idx row.description "Description"
      let mbCongestionChargeMultiplierValue :: (Maybe Centesimal) = readMaybeCSVField idx row.congestionChargeMultiplier "Congestion Charge Multiplier"
      let congestionChargeMultiplier =
            case mbCongestionChargeMultiplierValue of
              Nothing -> Nothing
              Just congestionChargeMultiplierValue -> do
                let congestionChargeMultiplierIncludeBaseFare :: Bool = (mapToBool . T.toLower) row.congestionChargeMultiplierIncludeBaseFare
                if congestionChargeMultiplierIncludeBaseFare
                  then Just $ FarePolicy.BaseFareAndExtraDistanceFare congestionChargeMultiplierValue
                  else Just $ FarePolicy.ExtraDistanceFare congestionChargeMultiplierValue
      let parkingCharge :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.parkingCharge "Parking Charge"
      let perStopCharge :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.perStopCharge "Per Stop Charge"
      currency :: Currency <- readCSVField idx row.currency "Currency"

      (freeWatingTime, waitingCharges, mbNightCharges) <- do
        freeWatingTime :: Minutes <- readCSVField idx row.freeWatingTime "Free Waiting Time"
        waitingCharge :: HighPrecMoney <- readCSVField idx row.waitingCharge "Waiting Charge"
        waitingChargeType <- cleanCSVField idx row.waitingChargeType "Waiting Charge Type"
        let waitingCharges =
              case waitingChargeType of
                "PerMinuteWaitingCharge" -> FarePolicy.PerMinuteWaitingCharge waitingCharge
                "ConstantWaitingCharge" -> FarePolicy.ConstantWaitingCharge waitingCharge
                _ -> FarePolicy.PerMinuteWaitingCharge waitingCharge
        let nightShiftChargeType = cleanMaybeCSVField idx row.nightShiftChargeType "Night Shift Charge Type"
        mbNightCharges <-
          case nightShiftChargeType of
            Just "ProgressiveNightShiftCharge" -> do
              nightShiftCharge :: Float <- readCSVField idx row.nightShiftCharge "Night Shift Charge"
              return (Just $ FarePolicy.ProgressiveNightShiftCharge nightShiftCharge)
            Just "ConstantNightShiftCharge" -> do
              nightShiftCharge :: HighPrecMoney <- readCSVField idx row.nightShiftCharge "Night Shift Charge"
              return (Just $ FarePolicy.ConstantNightShiftCharge nightShiftCharge)
            _ -> return Nothing
        return (freeWatingTime, waitingCharges, mbNightCharges)

      -- TODO: Add support for insurance charge and card charges in csv file
      let perDistanceUnitInsuranceCharge = Nothing
          cardCharge =
            Just
              FarePolicy.CardCharge
                { perDistanceUnitMultiplier = Nothing,
                  fixed = Nothing
                }

      cancellationFarePolicyId <- do
        let cancellationFarePolicyDescription :: Maybe Text = readMaybeCSVField idx row.cancellationFarePolicyDescription "Cancellation Fare Policy Description"
        case cancellationFarePolicyDescription of
          Nothing -> return Nothing
          Just cancellationFarePolicyDesc -> do
            freeCancellationTimeSeconds :: Seconds <- readCSVField idx row.freeCancellationTimeSeconds "Free Cancellation Time Seconds"
            maxCancellationCharge :: HighPrecMoney <- readCSVField idx row.maxCancellationCharge "Max Cancellation Charge"
            maxWaitingTimeAtPickupSeconds :: Seconds <- readCSVField idx row.maxWaitingTimeAtPickupSeconds "Max Waiting Time At Pickup Seconds"
            minCancellationCharge :: HighPrecMoney <- readCSVField idx row.minCancellationCharge "Min Cancellation Charge"
            perMetreCancellationCharge :: HighPrecMoney <- readCSVField idx row.perMetreCancellationCharge "Per Metre Cancellation Charge"
            perMinuteCancellationCharge :: HighPrecMoney <- readCSVField idx row.perMinuteCancellationCharge "Per Minute Cancellation Charge"
            percentageOfRideFareToBeCharged :: Centesimal <- readCSVField idx row.percentageOfRideFareToBeCharged "Percentage Of Ride Fare To Be Charged"
            newId <- generateGUID
            let cancellationFarePolicy =
                  DTCFP.CancellationFarePolicy
                    { id = newId,
                      description = cancellationFarePolicyDesc,
                      freeCancellationTimeSeconds,
                      maxCancellationCharge,
                      maxWaitingTimeAtPickupSeconds,
                      minCancellationCharge,
                      perMetreCancellationCharge,
                      perMinuteCancellationCharge,
                      percentageOfRideFareToBeCharged,
                      currency,
                      createdAt = now,
                      updatedAt = now
                    }
            QCFP.create cancellationFarePolicy
            return (Just newId)

      let mbPlatformFeeInfo = do
            let platformFeeType :: Maybe Text = cleanMaybeCSVField idx row.platformFeeChargeType "Platform Fee Charge Type"
                platformFeeCharge :: Maybe HighPrecMoney = readMaybeCSVField idx row.platformFeeCharge "Platform Fee Charge"
                platformFeeCgst :: Maybe Double = readMaybeCSVField idx row.platformFeeCgst "Platform Fee CGST %"
                platformFeeSgst :: Maybe Double = readMaybeCSVField idx row.platformFeeSgst "Platform Fee SGST %"
            case (platformFeeType, platformFeeCharge, platformFeeCgst, platformFeeSgst) of
              (Just "ProgressivePlatformFee", Just charge, Just cgstCharge, Just sgstCharge) -> Just $ FarePolicy.PlatformFeeInfo (FarePolicy.ProgressivePlatformFee charge) cgstCharge sgstCharge
              (Just "ConstantPlatformFee", Just charge, Just cgstCharge, Just sgstCharge) -> Just $ FarePolicy.PlatformFeeInfo (FarePolicy.ConstantPlatformFee charge) cgstCharge sgstCharge
              (_, _, _, _) -> Nothing

      farePolicyDetails <-
        case farePolicyType of
          FarePolicy.Progressive -> do
            baseDistance :: Meters <- readCSVField idx row.baseDistance "Base Distance"
            baseFare :: HighPrecMoney <- readCSVField idx row.baseFare "Base Fare"
            deadKmFare :: HighPrecMoney <- readCSVField idx row.deadKmFare "Dead Km Fare"
            let waitingChargeInfo =
                  Just
                    FarePolicy.WaitingChargeInfo
                      { waitingCharge = waitingCharges,
                        freeWaitingTime = freeWatingTime
                      }
            startDistance :: Meters <- readCSVField idx row.extraKmRateStartDistance "Extra Km Rate Start Distance"
            perExtraKmRate :: HighPrecMoney <- readCSVField idx row.perExtraKmRate "Per Extra Km Rate"
            let baseFareDepreciation :: HighPrecMoney = fromMaybe (HighPrecMoney 0.0) (readMaybeCSVField idx row.baseFareDepreciation "Base fare depreciation")
            let perExtraKmRateSections = NE.fromList [FarePolicy.FPProgressiveDetailsPerExtraKmRateSection {startDistance, distanceUnit, perExtraKmRate, baseFareDepreciation}]
            -- TODO: Add support for per min rate sections in csv file
            let perMinRateSections = Nothing
            return $ FarePolicy.ProgressiveDetails FarePolicy.FPProgressiveDetails {nightShiftCharge = mbNightCharges, ..}
          FarePolicy.Slabs -> do
            baseDistance :: Meters <- readCSVField idx row.baseDistance "Base Distance"
            baseFare :: HighPrecMoney <- readCSVField idx row.baseFare "Base Fare"
            let waitingChargeInfo =
                  Just
                    FarePolicy.WaitingChargeInfo
                      { waitingCharge = waitingCharges,
                        freeWaitingTime = freeWatingTime
                      }
            let slabs = NE.fromList [FarePolicy.FPSlabsDetailsSlab {startDistance = baseDistance, nightShiftCharge = mbNightCharges, platformFeeInfo = mbPlatformFeeInfo, ..}]
            return $ FarePolicy.SlabsDetails FarePolicy.FPSlabsDetails {slabs}
          FarePolicy.Rental -> do
            baseFare :: HighPrecMoney <- readCSVField idx row.baseFare "Base Fare"
            deadKmFare :: HighPrecMoney <- readCSVField idx row.deadKmFare "Dead Km Fare"
            perHourCharge :: HighPrecMoney <- readCSVField idx row.perHourCharge "Per Hour Charge"
            perExtraMinRate :: HighPrecMoney <- readCSVField idx row.perExtraMinRate "Per Extra Min Rate"
            perExtraKmRate :: HighPrecMoney <- readCSVField idx row.perExtraKmRate "Per Extra Km Rate"
            includedKmPerHr :: Kilometers <- readCSVField idx row.includedKmPerHr "Included Km Per Hour"
            plannedPerKmRate :: HighPrecMoney <- readCSVField idx row.plannedPerKmRate "Planned Per Km Rate"
            maxAdditionalKmsLimit :: Kilometers <- readCSVField idx row.maxAdditionalKmsLimit "Max Additional Kms Limit"
            totalAdditionalKmsLimit :: Kilometers <- readCSVField idx row.totalAdditionalKmsLimit "Total Additional Kms Limit"

            -- distanceBuffers
            rideDuration :: Seconds <- readCSVField idx row.rideDuration "Ride Duration"
            bufferKms :: Int <- readCSVField idx row.bufferKms "Buffer Kms"
            bufferMeters :: Int <- readCSVField idx row.bufferMeters "Buffer Meters"

            -- RentalpricingSlabs
            timePercentage :: Int <- readCSVField idx row.timePercentage "Time Percentage"
            distancePercentage :: Int <- readCSVField idx row.distancePercentage "Distance Percentage"
            farePercentage :: Int <- readCSVField idx row.farePercentage "Fare Percentage"
            includeActualTimePercentage :: Bool <- readCSVField idx row.includeActualTimePercentage "Include ActualTime Percentage"
            includeActualDistPercentage :: Bool <- readCSVField idx row.includeActualDistPercentage "Include Actual Dist Percentage"

            let waitingChargeInfo =
                  Just
                    FarePolicy.WaitingChargeInfo
                      { waitingCharge = waitingCharges,
                        freeWaitingTime = freeWatingTime
                      }
            let distanceBuffers = NE.fromList [FPRDDB.FPRentalDetailsDistanceBuffers {..}]
            let pricingSlabs = NE.fromList [FPRDPS.FPRentalDetailsPricingSlabs {..}]
            return $ FarePolicy.RentalDetails FarePolicy.FPRentalDetails {nightShiftCharge = mbNightCharges, ..}
          FarePolicy.InterCity -> do
            baseFare :: HighPrecMoney <- readCSVField idx row.baseFare "Base Fare"
            deadKmFare :: HighPrecMoney <- readCSVField idx row.deadKmFare "Dead Km Fare"
            perHourCharge :: HighPrecMoney <- readCSVField idx row.perHourCharge "Per Hour Charge Amount"
            perExtraMinRate :: HighPrecMoney <- readCSVField idx row.perExtraMinRate "Per Extra Min Rate"
            perExtraKmRate :: HighPrecMoney <- readCSVField idx row.perExtraKmRate "Per Extra Km Rate"
            perKmRateOneWay :: HighPrecMoney <- readCSVField idx row.perKmRateOneWay "Per Km Rate One Way"
            kmPerPlannedExtraHour :: Kilometers <- readCSVField idx row.kmPerPlannedExtraHour "Km Per Planned Extra Hour"
            perDayMaxHourAllowance :: Hours <- readCSVField idx row.perDayMaxHourAllowance "Per Day Max Hour Allowance"
            perDayMaxAllowanceInMins :: Minutes <- readCSVField idx row.perDayMaxAllowanceInMins "Per Day Max Allowance In Mins"
            perKmRateRoundTrip :: HighPrecMoney <- readCSVField idx row.perKmRateRoundTrip "Per Km Rate Round Trip"
            defaultWaitTimeAtDestination :: Minutes <- readCSVField idx row.defaultWaitTimeAtDestination "Default Wait Time At Destination"
            let stateEntryPermitCharges :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.stateEntryPermitCharges "State Entry Permit Charges"
            let mbPerDayMaxAllowanceInMins = Just perDayMaxAllowanceInMins
            -- InterCityPricingSlabs
            timePercentage :: Int <- readCSVField idx row.timePercentage "Time Percentage"
            distancePercentage :: Int <- readCSVField idx row.distancePercentage "Distance Percentage"
            farePercentage :: Int <- readCSVField idx row.farePercentage "Fare Percentage"
            includeActualTimePercentage :: Bool <- readCSVField idx row.includeActualTimePercentage "Include ActualTime Percentage"
            includeActualDistPercentage :: Bool <- readCSVField idx row.includeActualDistPercentage "Nnclude Actual Dist Percentage"

            let waitingChargeInfo =
                  Just
                    FarePolicy.WaitingChargeInfo
                      { waitingCharge = waitingCharges,
                        freeWaitingTime = freeWatingTime
                      }

            let pricingSlabs = NE.fromList [FPIDPS.FPInterCityDetailsPricingSlabs {..}]
            return $ FarePolicy.InterCityDetails FarePolicy.FPInterCityDetails {nightShiftCharge = mbNightCharges, perDayMaxAllowanceInMins = mbPerDayMaxAllowanceInMins, ..}
          _ -> throwError $ InvalidRequest "Fare Policy Type not supported"

      driverExtraFeeBounds <- do
        let mbStartDistance :: Maybe Meters = readMaybeCSVField idx row.startDistanceDriverAddition "Start Distance Driver Addition"
        case mbStartDistance of
          Nothing -> return Nothing
          Just startDistance -> do
            minFee :: HighPrecMoney <- readCSVField idx row.minFee "Min Fee"
            maxFee :: HighPrecMoney <- readCSVField idx row.maxFee "Max Fee"
            stepFee :: HighPrecMoney <- readCSVField idx row.stepFee "Step Fee"
            defaultStepFee :: HighPrecMoney <- readCSVField idx row.defaultStepFee "Default Step Fee"
            return $ NE.nonEmpty [DFPEFB.DriverExtraFeeBounds {..}]

      return ((Just . mapToBool) row.disableRecompute, city, vehicleServiceTier, tripCategory, area, timeBound, searchSource, enabled, FarePolicy.FarePolicy {id = Id idText, description = Just description, platformFee = platformFeeChargeFarePolicyLevel, sgst = platformFeeSgstFarePolicyLevel, cgst = platformFeeCgstFarePolicyLevel, platformFeeChargesBy = fromMaybe FarePolicy.Subscription platformFeeChargesBy, additionalCongestionCharge = 0, merchantId = Just merchantId, merchantOperatingCityId = Just merchantOpCity, ..})

    validateFarePolicyType farePolicyType = \case
      InterCity _ _ -> unless (farePolicyType `elem` [FarePolicy.InterCity, FarePolicy.Progressive]) $ throwError $ InvalidRequest "Fare Policy Type not supported for intercity"
      Rental _ -> unless (farePolicyType == FarePolicy.Rental) $ throwError $ InvalidRequest "Fare Policy Type not supported for rental"
      _ -> pure ()

    makeKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> TripCategory -> SL.Area -> DFareProduct.SearchSource -> Text
    makeKey cityId vehicleServiceTier tripCategory area searchSource =
      T.intercalate ":" [cityId.getId, show vehicleServiceTier, show area, show tripCategory, show searchSource]

    markBoundedAreadyDeleted :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> TripCategory -> SL.Area -> DFareProduct.SearchSource -> Map.Map Text Bool -> Map.Map Text Bool
    markBoundedAreadyDeleted cityId vehicleServiceTier tripCategory area searchSource mapObj = do
      let key = makeKey cityId vehicleServiceTier tripCategory area searchSource
      Map.insert key True mapObj

---------------------------------------------------------------------
postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId _city mbSpecialLocationId request = do
  existingSLWithGeom <- maybe (return Nothing) (Esq.runInReplica . QSL.findByIdWithGeom) mbSpecialLocationId
  let mbExistingSL = fst <$> existingSLWithGeom
      mbGeom = snd =<< existingSLWithGeom
  updatedSL <- mkSpecialLocation mbExistingSL mbGeom
  void $
    runTransaction $
      if isJust mbExistingSL then QSLG.updateSpecialLocation updatedSL else QSLG.create updatedSL
  return Success
  where
    mkSpecialLocation :: Maybe SL.SpecialLocation -> Maybe Text -> Flow SL.SpecialLocation
    mkSpecialLocation mbExistingSpLoc mbGeometry = do
      let geom = request.geom <|> mbGeometry
      id <- maybe generateGUID (return . (.id)) mbExistingSpLoc
      now <- getCurrentTime
      (merchantOperatingCityId, merchantId) <- case request.city of
        Just opCity -> do
          merchantOperatingCity <-
            CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
              >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
          let merchantOperatingCityId = cast @DMOC.MerchantOperatingCity @SL.MerchantOperatingCity merchantOperatingCity.id
              merchantId = cast @DM.Merchant @SL.Merchant merchantOperatingCity.merchantId
          pure (merchantOperatingCityId, merchantId)
        Nothing -> case (mbExistingSpLoc >>= (.merchantOperatingCityId), mbExistingSpLoc >>= (.merchantId)) of
          (Just merchantOperatingCityId, Just merchantId) -> pure (merchantOperatingCityId, merchantId)
          (Just merchantOperatingCityId, Nothing) -> do
            merchantOperatingCity <-
              CQMOC.findById (cast @SL.MerchantOperatingCity @DMOC.MerchantOperatingCity merchantOperatingCityId)
                >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId-" <> merchantOperatingCityId.getId)
            let merchantId = cast @DM.Merchant @SL.Merchant merchantOperatingCity.merchantId
            pure (merchantOperatingCityId, merchantId)
          (Nothing, _) -> throwError (InvalidRequest "Valid city should be provided")
      locationName <-
        fromMaybeM (InvalidRequest "Location Name cannot be empty for a new special location") $
          request.locationName <|> (mbExistingSpLoc <&> (.locationName))
      category <- fromMaybeM (InvalidRequest "Category is a required field for a new special location") $ request.category <|> (mbExistingSpLoc <&> (.category))
      return $
        SL.SpecialLocation
          { gates = [],
            createdAt = maybe now (.createdAt) mbExistingSpLoc,
            updatedAt = now,
            merchantOperatingCityId = Just merchantOperatingCityId,
            linkedLocationsIds = maybe [] (.linkedLocationsIds) mbExistingSpLoc,
            locationType = SL.Closed,
            merchantId = Just merchantId,
            ..
          }

deleteMerchantSpecialLocationDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Flow APISuccess
deleteMerchantSpecialLocationDelete _merchantShortid _city specialLocationId = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Special Location with given id not found")
  void $ runTransaction $ QSL.deleteById specialLocationId
  void $ runTransaction $ QGI.deleteAll specialLocationId
  pure Success

postMerchantSpecialLocationGatesUpsert :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Flow APISuccess
postMerchantSpecialLocationGatesUpsert _merchantShortId _city specialLocationId request = do
  specialLocation <- QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  createOrUpdateGate specialLocation existingGates request
  return Success
  where
    createOrUpdateGate :: SL.SpecialLocation -> [(D.GateInfo, Maybe Text)] -> Common.UpsertSpecialLocationGateReqT -> Flow ()
    createOrUpdateGate specialLocation existingGates req = do
      let existingGateWithGeom = find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName req.name) existingGates
          existingGate = fst <$> existingGateWithGeom
          mbGeom = snd =<< existingGateWithGeom
      updatedGate <- mkGate specialLocation req existingGate mbGeom
      void $
        runTransaction $
          if isNothing existingGate then QGIG.create updatedGate else QGIG.updateGate updatedGate

    mkGate :: SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Maybe D.GateInfo -> Maybe Text -> Flow D.GateInfo
    mkGate specialLocation reqT mbGate mbGeom = do
      id <- cast <$> maybe generateGUID (return . (.id)) mbGate
      now <- getCurrentTime
      latitude <- fromMaybeM (InvalidRequest "Latitude cannot be empty for a new gate") $ reqT.latitude <|> (mbGate <&> (.point.lat))
      longitude <- fromMaybeM (InvalidRequest "Longitude cannot be empty for a new gate") $ reqT.longitude <|> (mbGate <&> (.point.lon))
      address <- fromMaybeM (InvalidRequest "Address cannot be empty for a new gate") $ reqT.address <|> (mbGate >>= (.address))
      let canQueueUpOnGate = fromMaybe False $ reqT.canQueueUpOnGate <|> (mbGate <&> (.canQueueUpOnGate))
          defaultDriverExtra = reqT.defaultDriverExtra <|> (mbGate >>= (.defaultDriverExtra))
          geom = reqT.geom <|> mbGeom
      return $
        D.GateInfo
          { name = reqT.name,
            address = Just address,
            createdAt = maybe now (.createdAt) mbGate,
            updatedAt = now,
            point = LatLong {lat = latitude, lon = longitude},
            gateType = D.Pickup,
            merchantId = specialLocation.merchantId,
            merchantOperatingCityId = specialLocation.merchantOperatingCityId,
            ..
          }

deleteMerchantSpecialLocationGatesDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> Flow APISuccess
deleteMerchantSpecialLocationGatesDelete _merchantShortId _city specialLocationId gateName = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  let existingGate = fst <$> find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName gateName) existingGates
  case existingGate of
    Nothing -> throwError $ InvalidRequest "Could not find any gates with the specified name for the given specialLocationId"
    Just gate -> runTransaction $ QGI.deleteById gate.id
  return Success

normalizeName :: Text -> Text
normalizeName = T.strip . T.toLower

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId city req = do
  when (req.city == Context.AnyCity) $ throwError $ InvalidRequest "This Operation is not Allowed For AnyCity"
  baseMerchant <- findMerchantByShortId merchantShortId
  let baseMerchantId = baseMerchant.id
  baseOperatingCityId <- CQMOC.getMerchantOpCityId Nothing baseMerchant (Just city)
  now <- getCurrentTime

  let newMerchantId =
        case req.merchantData of
          Just merchantData -> Id merchantData.subscriberId
          Nothing -> baseMerchantId

  -- merchant
  mbNewMerchant <-
    case req.merchantData of
      Just merchantData -> do
        CQM.findById newMerchantId >>= \case
          Nothing -> do
            merchant <- CQM.findById baseMerchantId >>= fromMaybeM (InvalidRequest "Base Merchant not found")
            let newMerchant = buildMerchant newMerchantId merchantData now merchant
            return $ Just newMerchant
          _ -> return Nothing
      _ -> return Nothing

  cityAlreadyCreated <- CQMOC.findByMerchantIdAndCity newMerchantId req.city

  newMerchantOperatingCityId :: Id MerchantOperatingCity <-
    case cityAlreadyCreated of
      Just newCity -> return newCity.id
      Nothing -> generateGUID

  -- city
  baseOperatingCity <- CQMOC.findById baseOperatingCityId >>= fromMaybeM (InvalidRequest "Base Operating City not found")
  let newMerchantShortId = maybe merchantShortId (.shortId) mbNewMerchant
  let mbNewOperatingCity =
        case cityAlreadyCreated of
          Nothing -> Just $ buildMerchantOperatingCity newMerchantId baseOperatingCity newMerchantOperatingCityId newMerchantShortId
          _ -> Nothing

  -- intelligent pool config
  mbInteglligentPoolConfig <-
    CDIPC.findByMerchantOpCityId newMerchantOperatingCityId Nothing >>= \case
      Nothing -> do
        intelligentPoolConfig <- CDIPC.findByMerchantOpCityId baseOperatingCityId Nothing >>= fromMaybeM (InvalidRequest "Intelligent Pool Config not found")
        let newIntelligentPoolConfig = buildIntelligentPoolConfig newMerchantId newMerchantOperatingCityId now intelligentPoolConfig
        return $ Just newIntelligentPoolConfig
      _ -> return Nothing

  -- driver pool config
  mbDriverPoolConfigs <-
    CQDPC.findAllByMerchantOpCityId newMerchantOperatingCityId (Just []) Nothing >>= \case
      [] -> do
        driverPoolConfigs <- CQDPC.findAllByMerchantOpCityId baseOperatingCityId (Just []) Nothing
        newDriverPoolConfigs <- mapM (buildPoolConfig newMerchantId newMerchantOperatingCityId now) driverPoolConfigs
        return $ Just newDriverPoolConfigs
      _ -> return Nothing

  -- fare products
  mbFareProducts <-
    CQFProduct.findAllFareProductByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        fareProducts <- CQFProduct.findAllFareProductByMerchantOpCityId baseOperatingCityId
        newFareProducts <- mapM (buildFareProduct newMerchantId newMerchantOperatingCityId) fareProducts
        return $ Just newFareProducts
      _ -> return Nothing

  -- vehicle service tier
  mbVehicleServiceTier <-
    CQVST.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId baseOperatingCityId
        newVehicleServiceTiers <- mapM (buildVehicleServiceTier newMerchantId newMerchantOperatingCityId) vehicleServiceTiers
        return $ Just newVehicleServiceTiers
      _ -> return Nothing

  -- go home config
  mbGoHomeConfig <-
    try @_ @SomeException (CGHC.findByMerchantOpCityId newMerchantOperatingCityId Nothing) >>= \case
      Left _ -> do
        goHomeConfig <- CGHC.findByMerchantOpCityId baseOperatingCityId Nothing
        let newGoHomeConfig = buildGoHomeConfig newMerchantId newMerchantOperatingCityId now goHomeConfig
        return $ Just newGoHomeConfig
      Right _ -> return Nothing

  -- leader board configs
  mbLeaderBoardConfig <-
    CQLBC.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        leaderBoardConfigs <- CQLBC.findAllByMerchantOpCityId baseOperatingCityId
        newLeaderBoardConfigs <- mapM (buildLeaderBoardConfig newMerchantId newMerchantOperatingCityId) leaderBoardConfigs
        return $ Just newLeaderBoardConfigs
      _ -> return Nothing

  -- merchant message
  mbMerchantMessages <-
    CQMM.findAllByMerchantOpCityId newMerchantOperatingCityId Nothing >>= \case
      [] -> do
        merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId Nothing
        let newMerchantMessages = map (buildMerchantMessage newMerchantId newMerchantOperatingCityId now) merchantMessages
        return $ Just newMerchantMessages
      _ -> return Nothing

  -- merchant overlay
  mbMerchantOverlays <-
    CQMO.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantOverlays <- CQMO.findAllByMerchantOpCityId baseOperatingCityId
        newMerchantOverlays <- mapM (buildMerchantOverlay newMerchantId newMerchantOperatingCityId) merchantOverlays
        return $ Just newMerchantOverlays
      _ -> return Nothing

  -- merchant payment method
  mbMerchantPaymentMethods <-
    CQMPM.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId baseOperatingCityId
        newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newMerchantId newMerchantOperatingCityId now) merchantPaymentMethods
        return $ Just newMerchantPaymentMethods
      _ -> return Nothing

  -- merchant service usage config
  mbMerchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId newMerchantOperatingCityId Nothing >>= \case
      Nothing -> do
        merchantServiceUsageConfig <- CQMSUC.findByMerchantOpCityId baseOperatingCityId Nothing >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
        let newMerchantServiceUsageConfig = buildMerchantServiceUsageConfig newMerchantId newMerchantOperatingCityId now merchantServiceUsageConfig
        return $ Just newMerchantServiceUsageConfig
      _ -> return Nothing

  -- merchant service config
  mbMerchantServiceConfigs <-
    CQMSC.findAllMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantServiceConfigs <- CQMSC.findAllMerchantOpCityId baseOperatingCityId
        let newMerchantServiceConfigs = map (buildMerchantServiceConfig newMerchantId newMerchantOperatingCityId now) merchantServiceConfigs
        return $ Just newMerchantServiceConfigs
      _ -> return Nothing

  -- merchant push notification
  mbMerchantPushNotification <-
    CQMPN.findAllByMerchantOpCityIdInRideFlow newMerchantOperatingCityId [] >>= \case
      [] -> do
        merchantPushNotification <- CQMPN.findAllByMerchantOpCityIdInRideFlow baseOperatingCityId []
        newMerchantPushNotifications <- mapM (buildMerchantPushNotification newMerchantId newMerchantOperatingCityId now) merchantPushNotification
        return $ Just newMerchantPushNotifications
      _ -> return Nothing

  -- onboarding document config
  mbDocumentVerificationConfigs <-
    CQDVC.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        documentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId baseOperatingCityId
        let newDocumentVerificationConfigs = map (buildNewDocumentVerificationConfig newMerchantId newMerchantOperatingCityId now) documentVerificationConfigs
        return $ Just newDocumentVerificationConfigs
      _ -> return Nothing

  -- payout config
  mbPayoutConfigs <-
    CPC.findAllByMerchantOpCityId newMerchantOperatingCityId Nothing >>= \case
      [] -> do
        payoutConfigs <- CPC.findAllByMerchantOpCityId baseOperatingCityId Nothing
        let newPayoutConfigs = map (buildPayoutConfig newMerchantId newMerchantOperatingCityId now) payoutConfigs
        return $ Just newPayoutConfigs
      _ -> return Nothing

  -- transporter config
  mbTransporterConfig <-
    CTC.findByMerchantOpCityId newMerchantOperatingCityId Nothing >>= \case
      Nothing -> do
        transporterConfig <- CTC.findByMerchantOpCityId baseOperatingCityId Nothing >>= fromMaybeM (InvalidRequest "Transporter Config not found")
        let newTransporterConfig = buildTransporterConfig newMerchantId newMerchantOperatingCityId now transporterConfig
        return $ Just newTransporterConfig
      Just _ -> return Nothing

  -- geometry
  mbGeometry <-
    QGEO.findGeometryByStateAndCity req.city req.state >>= \case
      Nothing -> do
        Just <$> buildGeometry
      _ -> return Nothing

  -- call ride exophone
  mbExophone <-
    CQExophone.findAllCallExophoneByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        exophones <- CQExophone.findAllCallExophoneByMerchantOpCityId baseOperatingCityId
        return $ Just exophones
      _ -> return Nothing

  -- issue config
  mbIssueConfig <-
    CQIssueConfig.findByMerchantOpCityId (cast newMerchantOperatingCityId) ICommon.DRIVER >>= \case
      Nothing -> do
        issueConfig <- CQIssueConfig.findByMerchantOpCityId (cast baseOperatingCityId) ICommon.DRIVER >>= fromMaybeM (InvalidRequest "Issue Config not found")
        newIssueConfig <- buildIssueConfig newMerchantId newMerchantOperatingCityId now issueConfig
        return $ Just newIssueConfig
      _ -> return Nothing

  -- beckn config
  mbBecknConfig <-
    case req.merchantData of
      Just _ -> do
        SQBC.findAllByMerchantId (Just newMerchantId) >>= \case
          [] -> do
            becknConfig <- SQBC.findAllByMerchantId (Just baseMerchantId)
            newBecknConfig <- mapM (buildBecknConfig newMerchantId now) becknConfig
            return $ Just newBecknConfig
          _ -> return Nothing
      Nothing -> return Nothing

  -- subscription config
  subscriptionConfigs <-
    mapM (CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (cast newMerchantOperatingCityId)) [Plan.YATRI_SUBSCRIPTION, Plan.YATRI_RENTAL] >>= \cfgs -> do
      subscriptionCfgs <- mapM (CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (cast baseOperatingCityId)) $
        case cfgs of
          [Nothing, Nothing] -> [Plan.YATRI_SUBSCRIPTION, Plan.YATRI_RENTAL]
          [Nothing, _] -> [Plan.YATRI_SUBSCRIPTION]
          [_, Nothing] -> [Plan.YATRI_RENTAL]
          _ -> []
      return $ map (buildSubscriptionConfig newMerchantId newMerchantOperatingCityId now <$>) subscriptionCfgs

  nyRegistryUrl <- asks (.nyRegistryUrl)
  let uniqueKeyId = baseMerchant.uniqueKeyId
      subscriberId = baseMerchant.subscriberId.getShortId
      subType = BecknSub.BPP
      domain = Context.MOBILITY
      lookupReq = SimpleLookupRequest {unique_key_id = uniqueKeyId, subscriber_id = subscriberId, merchant_id = baseMerchant.id.getId, subscriber_type = subType, ..}
      newUniqueId = maybe uniqueKeyId (.uniqueKeyId) mbNewMerchant
      newSubscriberId = maybe subscriberId (.subscriberId.getShortId) mbNewMerchant
  mbAddCityReq <-
    Registry.registryLookup nyRegistryUrl lookupReq subscriberId >>= \case
      Nothing -> do
        logError $ "No entry found for subscriberId: " <> subscriberId <> ", uniqueKeyId: " <> uniqueKeyId <> " in NY registry"
        return Nothing
      Just sub | req.city `elem` sub.city -> return Nothing
      Just _ -> Just <$> RegistryT.buildAddCityNyReq (req.city :| []) newUniqueId newSubscriberId subType domain

  finally
    ( do
        whenJust mbGeometry $ \geometry -> QGEO.create geometry
        whenJust mbNewMerchant $ \newMerchant -> QM.create newMerchant
        whenJust mbNewOperatingCity $ \newOperatingCity -> CQMOC.create newOperatingCity
        whenJust mbInteglligentPoolConfig $ \newIntelligentPoolConfig -> CQDIPC.create newIntelligentPoolConfig
        whenJust mbDriverPoolConfigs $ \newDriverPoolConfigs -> mapM_ CQDPC.create newDriverPoolConfigs
        whenJust mbFareProducts $ \newFareProducts -> mapM_ CQFProduct.create newFareProducts
        whenJust mbVehicleServiceTier $ \newVehicleServiceTiers -> CQVST.createMany newVehicleServiceTiers
        whenJust mbGoHomeConfig $ \newGoHomeConfig -> CQGHC.create newGoHomeConfig
        whenJust mbLeaderBoardConfig $ \newLeaderBoardConfigs -> mapM_ CQLBC.create newLeaderBoardConfigs
        whenJust mbMerchantMessages $ \newMerchantMessages -> mapM_ CQMM.create newMerchantMessages
        whenJust mbMerchantOverlays $ \newMerchantOverlays -> mapM_ CQMO.create newMerchantOverlays
        whenJust mbMerchantPaymentMethods $ \newMerchantPaymentMethods -> mapM_ CQMPM.create newMerchantPaymentMethods
        whenJust mbMerchantServiceUsageConfig $ \newMerchantServiceUsageConfig -> CQMSUC.create newMerchantServiceUsageConfig
        whenJust mbMerchantServiceConfigs $ \newMerchantServiceConfigs -> mapM_ CQMSC.create newMerchantServiceConfigs
        whenJust mbMerchantPushNotification $ \newMerchantPushNotifications -> mapM_ CQMPN.create newMerchantPushNotifications
        whenJust mbDocumentVerificationConfigs $ \newDocumentVerificationConfigs -> mapM_ CQDVC.create newDocumentVerificationConfigs
        whenJust mbPayoutConfigs $ \newPayoutConfigs -> mapM_ CPC.create newPayoutConfigs
        whenJust mbTransporterConfig $ \newTransporterConfig -> CQTC.create newTransporterConfig
        whenJust mbBecknConfig $ \becknConfig -> mapM_ SQBC.create becknConfig
        mapM_ (`whenJust` QSC.create) subscriptionConfigs

        whenJust mbExophone $ \exophones ->
          whenJust (find (\exophone -> exophone.exophoneType == DExophone.CALL_RIDE) exophones) $ \exophone -> do
            exophone' <- buildNewExophone newMerchantId newMerchantOperatingCityId now exophone
            CQExophone.create exophone'
        whenJust mbIssueConfig $ \newIssueConfig -> CQIssueConfig.create newIssueConfig

        when (req.enableForMerchant) $ do
          let origin = maybe baseMerchant.geofencingConfig.origin (.geofencingConfig.origin) mbNewMerchant
              destination = maybe baseMerchant.geofencingConfig.destination (.geofencingConfig.destination) mbNewMerchant
              newOrigin = updateGeoRestriction origin
              newDestination = updateGeoRestriction destination

          when (checkGeofencingConfig origin && checkGeofencingConfig destination) $ do
            CQM.updateGeofencingConfig newMerchantId newOrigin newDestination
            CQM.clearCache $ fromMaybe baseMerchant mbNewMerchant

        whenJust mbAddCityReq $ \addCityReq ->
          void $ RegistryIF.updateSubscriber addCityReq
    )
    ( do
        CQDIPC.clearCache newMerchantOperatingCityId
        CQDPC.clearCache newMerchantOperatingCityId
        CQFProduct.clearCacheById newMerchantOperatingCityId
        CQVST.clearCache newMerchantOperatingCityId
        CQLBC.clearCache newMerchantOperatingCityId
        CQMM.clearCacheById newMerchantOperatingCityId
        CQMO.clearCache newMerchantOperatingCityId
        CQMPM.clearCache newMerchantOperatingCityId
        CQMSUC.clearCache newMerchantOperatingCityId
        CQMSC.clearCacheById newMerchantOperatingCityId
        CQMPN.clearCacheById newMerchantOperatingCityId
        CQDVC.clearCache newMerchantOperatingCityId
        CPC.clearCacheById newMerchantOperatingCityId
        CQTC.clearCache newMerchantOperatingCityId
        CQIssueConfig.clearIssueConfigCache (cast newMerchantOperatingCityId) ICommon.DRIVER
        exoPhone <- CQExophone.findAllCallExophoneByMerchantOpCityId newMerchantOperatingCityId
        CQExophone.clearCache newMerchantOperatingCityId exoPhone
        whenJust mbAddCityReq $ \_ -> Hedis.del $ cacheRegistryKey <> lookupRequestToRedisKey lookupReq
    )

  pure $ Common.CreateMerchantOperatingCityRes newMerchantOperatingCityId.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [show req.city]

    checkGeofencingConfig = \case
      Regions regions -> notElem (show req.city) regions
      Unrestricted -> True

    buildGeometry = do
      id <- generateGUID
      pure
        DGEO.Geometry
          { id,
            region = show req.city,
            state = req.state,
            city = req.city,
            geom = Just req.geom
          }

    buildMerchant merchantId merchantData currentTime DM.Merchant {city = _city, ..} =
      DM.Merchant
        { id = merchantId,
          subscriberId = ShortId merchantData.subscriberId,
          shortId = ShortId merchantData.shortId,
          name = merchantData.name,
          city = req.city,
          state = req.state,
          country = req.country,
          geofencingConfig =
            GeofencingConfig
              { origin = Regions [show req.city],
                destination = Regions [show req.city]
              },
          uniqueKeyId = merchantData.uniqueKeyId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantOperatingCity merchantId baseCity newCityId newMerchantShortId = do
      DMOC.MerchantOperatingCity
        { id = newCityId,
          merchantId,
          merchantShortId = newMerchantShortId,
          location = Maps.LatLong req.lat req.long,
          city = req.city,
          state = req.state,
          country = req.country,
          supportNumber = req.supportNumber <|> baseCity.supportNumber,
          language = fromMaybe baseCity.language req.primaryLanguage,
          currency = fromMaybe baseCity.currency req.currency,
          distanceUnit = fromMaybe baseCity.distanceUnit req.distanceUnit
        }

    buildIntelligentPoolConfig mId newCityId currentTime DDIPC.DriverIntelligentPoolConfig {..} =
      DDIPC.DriverIntelligentPoolConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildPoolConfig mId newCityId currentTime DDPC.DriverPoolConfig {..} = do
      newId <- generateGUID
      return $
        DDPC.DriverPoolConfig
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildNewExophone mId newCityId currentTime DExophone.Exophone {..} = do
      newId <- generateGUID
      return $
        DExophone.Exophone
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildFareProduct mId newCityId DFareProduct.FareProduct {..} = do
      newId <- generateGUID
      return $
        DFareProduct.FareProduct
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildVehicleServiceTier mId newCityId DVST.VehicleServiceTier {..} = do
      newId <- generateGUID
      return $
        DVST.VehicleServiceTier
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildGoHomeConfig mId newCityId currentTime DGoHomeConfig.GoHomeConfig {..} =
      DGoHomeConfig.GoHomeConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          enableGoHome = False,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildLeaderBoardConfig mId newCityId DLC.LeaderBoardConfigs {..} = do
      newId <- generateGUID
      return $
        DLC.LeaderBoardConfigs
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildMerchantMessage mId newCityId currentTime DMM.MerchantMessage {..} =
      DMM.MerchantMessage
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantOverlay mId newCityId DMO.Overlay {..} = do
      newId <- generateGUID
      return $
        DMO.Overlay
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildMerchantPaymentMethod mId newCityId currentTime DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      return $
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildMerchantServiceUsageConfig mId newCityId currentTime DMSUC.MerchantServiceUsageConfig {..} =
      DMSUC.MerchantServiceUsageConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantServiceConfig mId newCityId currentTime DMSC.MerchantServiceConfig {..} =
      DMSC.MerchantServiceConfig
        { merchantOperatingCityId = Just newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantPushNotification mId newCityId currentTime DMPN.MerchantPushNotification {..} = do
      newId <- generateGUID
      return $
        DMPN.MerchantPushNotification
          { merchantOperatingCityId = newCityId,
            merchantId = mId,
            createdAt = currentTime,
            updatedAt = currentTime,
            id = newId,
            ..
          }

    buildNewDocumentVerificationConfig mId newCityId currentTime DVC.DocumentVerificationConfig {..} =
      DVC.DocumentVerificationConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          rcNumberPrefixList = fromMaybe rcNumberPrefixList req.rcNumberPrefixList,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildPayoutConfig mId newCityId currentTime DPC.PayoutConfig {..} = do
      DPC.PayoutConfig
        { merchantOperatingCityId = newCityId,
          isPayoutEnabled = False,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildTransporterConfig mId newCityId currentTime DTC.TransporterConfig {..} =
      DTC.TransporterConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildIssueConfig mId newCityId currentTime DIConfig.IssueConfig {..} = do
      newId <- generateGUID
      return $
        DIConfig.IssueConfig
          { id = newId,
            merchantId = cast mId,
            merchantOperatingCityId = cast newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildBecknConfig newMerchantId currentTime DBC.BecknConfig {..} = do
      newId <- generateGUID
      let newSubscriberUrlText = maybe (showBaseUrl subscriberUrl) (\mId -> T.replace mId.getId newMerchantId.getId (showBaseUrl subscriberUrl)) merchantId
      newSubscriberUrl <- parseBaseUrl newSubscriberUrlText
      return
        DBC.BecknConfig
          { id = newId,
            merchantId = Just newMerchantId,
            merchantOperatingCityId = Nothing,
            subscriberId = maybe subscriberId (\mId -> T.replace mId.getId newMerchantId.getId subscriberId) merchantId,
            subscriberUrl = newSubscriberUrl,
            uniqueKeyId = fromMaybe uniqueKeyId (req.merchantData <&> (.uniqueKeyId)),
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }
    buildSubscriptionConfig newMerchantId newCityId currentTime DSC.SubscriptionConfig {..} =
      DSC.SubscriptionConfig
        { merchantId = Just newMerchantId,
          merchantOperatingCityId = Just newCityId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

mapToBool :: Text -> Bool
mapToBool = \case
  "yes" -> True
  "no" -> False
  "true" -> True
  "false" -> False
  _ -> False

replaceEmpty :: Text -> Maybe Text
replaceEmpty = \case
  "" -> Nothing
  "no constraint" -> Nothing
  "no_constraint" -> Nothing
  x -> Just x

data VehicleVariantMappingCSVRow = VehicleVariantMappingCSVRow
  { vehicleClass :: Text,
    vehicleCapacity :: Text,
    vehicleVariant :: Text,
    manufacturer :: Text,
    manufacturerModel :: Text,
    reviewRequired :: Text,
    vehicleModel :: Text,
    priority :: Text
  }

instance FromNamedRecord VehicleVariantMappingCSVRow where
  parseNamedRecord r =
    VehicleVariantMappingCSVRow
      <$> r .: "vehicle_class"
      <*> r .: "vehicle_capacity"
      <*> r .: "vehicle_variant"
      <*> r .: "manufacturer"
      <*> r .: "manufacturer_model"
      <*> r .: "review_required"
      <*> r .: "vehicle_model"
      <*> r .: "priority"

postMerchantUpdateOnboardingVehicleVariantMapping :: ShortId DM.Merchant -> Context.City -> Common.UpdateOnboardingVehicleVariantMappingReq -> Flow APISuccess
postMerchantUpdateOnboardingVehicleVariantMapping merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  logTagInfo "Updating onboarding vehicle variant mapping for merchant: " (show merchant.id <> " and city: " <> show opCity)
  configs <- readCsv req.file
  logTagInfo "Read file: " (show configs)
  CQDVC.updateSupportedVehicleClassesJSON merchantOpCity.id (DVC.RCValidClasses configs)
  logTagInfo "Read file Done" ""
  CQDVC.clearCache merchantOpCity.id
  return Success
  where
    readCsv csvFile = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector VehicleVariantMappingCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM makeConfig v >>= (pure . V.toList)

    makeConfig :: Int -> VehicleVariantMappingCSVRow -> Flow DVC.VehicleClassVariantMap
    makeConfig idx row = do
      let cleanField = replaceEmpty . T.toLower . T.strip
      vehicleVariant <- readMaybe (T.unpack row.vehicleVariant) & fromMaybeM (InvalidRequest $ "Invalid vehicle variant: " <> show row.vehicleVariant <> " at row: " <> show idx)
      vehicleClass <- cleanField row.vehicleClass & fromMaybeM (InvalidRequest $ "Vehicle class cannot be empty or without constraint: " <> show row.vehicleClass <> " at row: " <> show idx)
      vehicleModel <- replaceEmpty (T.strip row.vehicleModel) & fromMaybeM (InvalidRequest $ "Vehicle Model cannot be empty or without constraint: " <> show row.vehicleModel <> " at row: " <> show idx)
      return $
        DVC.VehicleClassVariantMap
          { vehicleClass,
            vehicleCapacity = cleanField row.vehicleCapacity >>= readMaybe . T.unpack,
            vehicleVariant,
            manufacturer = cleanField row.manufacturer,
            manufacturerModel = cleanField row.manufacturerModel,
            reviewRequired = cleanField row.reviewRequired <&> (mapToBool . T.toLower),
            vehicleModel = Just vehicleModel,
            priority = cleanField row.priority >>= readMaybe . T.unpack,
            bodyType = Nothing
          }

postMerchantConfigClearCacheSubscription :: ShortId DM.Merchant -> Context.City -> Common.ClearCacheSubscriptionReq -> Flow APISuccess
postMerchantConfigClearCacheSubscription merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  plans <- QPlan.fetchAllPlanByMerchantOperatingCityMbServiceName merchantOpCity.id (castServiceName <$> req.serviceName)
  let plansToClearCache = filter (filterCriteriaPlan req.serviceName (Id <$> req.planId)) plans
  forM_ plansToClearCache $ \plan -> do
    let keysToClear =
          [ CQPlan.makeAllPlanKey,
            CQPlan.makePlanIdAndPaymentModeKey plan.id plan.paymentMode plan.serviceName,
            CQPlan.makeMerchantIdAndPaymentModeKey plan.merchantOpCityId plan.paymentMode plan.serviceName (Just plan.isDeprecated),
            CQPlan.makeMerchantIdAndPaymentModeKey plan.merchantOpCityId plan.paymentMode plan.serviceName Nothing,
            CQPlan.makeMerchantIdAndTypeKey plan.merchantOpCityId plan.planType plan.serviceName plan.vehicleCategory False,
            CQPlan.makeMerchantIdKey plan.merchantOpCityId plan.serviceName,
            CQPlan.makeMerchantIdAndPaymentModeAndVariantKey plan.merchantOpCityId plan.paymentMode plan.serviceName plan.vehicleVariant (Just plan.isDeprecated),
            CQPlan.makeIdKey plan.merchantOpCityId plan.paymentMode plan.serviceName plan.vehicleCategory plan.isDeprecated,
            SPayment.makeOfferListCacheVersionKey
          ]
    forM_ keysToClear $ \key -> Hedis.del key
  return Success
  where
    filterCriteriaPlan mbServiceName mbPlanId =
      case (mbServiceName, mbPlanId) of
        (Just serviceName, Just planId) -> \plan -> plan.serviceName == castServiceName serviceName && plan.id == planId
        (Just serviceName, Nothing) -> \plan -> plan.serviceName == castServiceName serviceName
        (Nothing, Just planId) -> \plan -> plan.id == planId
        (Nothing, Nothing) -> \_ -> True
    castServiceName = \case
      Common.YATRI_RENTAL -> Plan.YATRI_RENTAL
      Common.YATRI_SUBSCRIPTION -> Plan.YATRI_SUBSCRIPTION

postMerchantConfigFailover :: ShortId DM.Merchant -> Context.City -> Common.ConfigNames -> Common.ConfigFailoverReq -> Flow APISuccess
postMerchantConfigFailover merchantShortId city configNames req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  case configNames of
    Common.BecknNetwork -> do
      configureBecknNetworkFailover merchant req
    _ -> do
      configureMessageProviderFailover merchantOperatingCity req
  pure Success

configureBecknNetworkFailover :: DM.Merchant -> Common.ConfigFailoverReq -> Flow ()
configureBecknNetworkFailover merchant req = do
  case req.priorityOrder of
    Just priorityOrder -> do
      CQM.updateGatewayAndRegistryPriorityList merchant (castNetworkEnums <$> priorityOrder.networkTypes)
    Nothing -> do
      let networkPriorityList = reorderList merchant.gatewayAndRegistryPriorityList
      CQM.updateGatewayAndRegistryPriorityList merchant networkPriorityList
  pure ()

configureMessageProviderFailover :: DMOC.MerchantOperatingCity -> Common.ConfigFailoverReq -> Flow ()
configureMessageProviderFailover merchantOperatingCity req = do
  merchantServiceUsageConfig <- CQMSUC.findByMerchantOpCityId merchantOperatingCity.id Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  case req.priorityOrder of
    Just priorityOrder -> do
      let smsProviders = fromMaybe merchantServiceUsageConfig.smsProvidersPriorityList (nonEmpty priorityOrder.smsProviders)
          whatsappProviders = fromMaybe merchantServiceUsageConfig.whatsappProvidersPriorityList (nonEmpty priorityOrder.whatsappProviders)
          updatedConfig = merchantServiceUsageConfig {DMSUC.smsProvidersPriorityList = smsProviders, DMSUC.whatsappProvidersPriorityList = whatsappProviders}
      CQMSUC.updateMerchantServiceUsageConfig updatedConfig
    Nothing -> do
      let messageProviderPriorityList = reorderList merchantServiceUsageConfig.smsProvidersPriorityList
          whatsappProviderPriorityList = reorderList merchantServiceUsageConfig.whatsappProvidersPriorityList
      let updatedConfig = merchantServiceUsageConfig {DMSUC.smsProvidersPriorityList = messageProviderPriorityList, DMSUC.whatsappProvidersPriorityList = whatsappProviderPriorityList}
      CQMSUC.updateMerchantServiceUsageConfig updatedConfig
  pure ()

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

reorderList :: [a] -> [a]
reorderList [] = []
reorderList (x : xs) = xs ++ [x]

castNetworkEnums :: Common.NetworkEnums -> Domain.Types.GatewayAndRegistryService
castNetworkEnums Common.ONDC = Domain.Types.ONDC
castNetworkEnums Common.NY = Domain.Types.NY

postMerchantPayoutConfigUpdate :: ShortId DM.Merchant -> Context.City -> Common.PayoutConfigReq -> Flow APISuccess
postMerchantPayoutConfigUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCity.id req.vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show req.vehicleCategory) merchantOpCity.id.getId)
  QPC.updateConfigValues req payoutConfig merchantOpCity.id
  CPC.clearConfigCache merchantOpCity.id req.vehicleCategory
  pure Success
