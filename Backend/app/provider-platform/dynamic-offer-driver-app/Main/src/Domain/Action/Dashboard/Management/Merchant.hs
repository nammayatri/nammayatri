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
    castCategory,
    castDVehicleVariant,
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
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Merchant as Common
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
import Domain.Types.CancellationFarePolicy as DTCFP
import qualified Domain.Types.Common as Common
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverIntelligentPoolConfig as DDIPC
import qualified Domain.Types.DriverPoolConfig as DDPC
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FarePolicy as FarePolicy
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DFPEFB
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.GoHomeConfig as DGoHomeConfig
import qualified Domain.Types.LeaderBoardConfigs as DLC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Overlay as DMO
import qualified Domain.Types.Plan as Plan
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.TimeBound
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment
import qualified EulerHS.Language as L
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Types.Value (MandatoryValue, OptionalValue)
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator (AllocatorJobType (..), BadDebtCalculationJobData, CalculateDriverFeesJobData, DriverReferralPayoutJobData)
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as DriverPool
import qualified SharedLogic.DriverFee as SDF
import qualified SharedLogic.DriverPool.Types as DriverPool
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Cac.DriverIntelligentPoolConfig as CDIPC
import qualified Storage.Cac.DriverIntelligentPoolConfig as CQDIPC
import qualified Storage.Cac.DriverPoolConfig as CQDPC
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.GoHomeConfig as CQGHC
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
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.Overlay as CQMO
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.CancellationFarePolicy as QCFP
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QFPEFB
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as QFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QFPPDEKM
import qualified Storage.Queries.Geometry as QGEO
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
postMerchantSchedulerTrigger merchantShortId _ req = do
  void $ findMerchantByShortId merchantShortId
  now <- getCurrentTime
  maxShards <- asks (.maxShards)
  case req.scheduledAt of
    Just utcTime -> do
      let diffTimeS = diffUTCTime utcTime now
      triggerScheduler req.jobName maxShards req.jobData diffTimeS
    _ -> throwError $ InternalError "invalid scheduled at time"
  where
    triggerScheduler :: Maybe Common.JobName -> Int -> Text -> NominalDiffTime -> Flow APISuccess
    triggerScheduler jobName maxShards jobDataRaw diffTimeS = do
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
              createJobIn @_ @'CalculateDriverFees diffTimeS maxShards (jobData :: CalculateDriverFeesJobData)
              setDriverFeeCalcJobCache jobData.startTime jobData.endTime merchantOpCityId serviceName diffTimeS
              setDriverFeeBillNumberKey merchantOpCityId 1 36000 serviceName
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        Just Common.BadDebtCalculationTrigger -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe BadDebtCalculationJobData
          case jobData' of
            Just jobData -> do
              createJobIn @_ @'BadDebtCalculation diffTimeS maxShards (jobData :: BadDebtCalculationJobData)
              pure Success
            Nothing -> throwError $ InternalError "invalid job data"
        Just Common.ReferralPayoutTrigger -> do
          let jobData' = decodeFromText jobDataRaw :: Maybe DriverReferralPayoutJobData
          case jobData' of
            Just jobData -> do
              createJobIn @_ @'DriverReferralPayout diffTimeS maxShards (jobData :: DriverReferralPayoutJobData)
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
    Nothing -> CQDPC.findAllByMerchantOpCityId merchantOpCityId
    Just tripDistance -> maybeToList <$> CQDPC.findByMerchantOpCityIdAndTripDistance merchantOpCityId tripDistance
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

---------------------------------------------------------------------
postMerchantConfigDriverPoolUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Maybe Common.Variant ->
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
  let serviceTier = DriverPool.castVariantToServiceTier <$> (castVehicleVariant <$> mbVariant)
  config <- CQDPC.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh merchantOpCityId tripDistance serviceTier tripCategory area >>= fromMaybeM (DriverPoolConfigDoesNotExist merchantOpCityId.getId tripDistance)
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
  Maybe Common.Variant ->
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
  let serviceTier = DriverPool.castVariantToServiceTier <$> (castVehicleVariant <$> mbVariant)
  mbConfig <- CQDPC.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh merchantOpCityId tripDistance serviceTier tripCategory area
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
  Maybe DVST.ServiceTierType ->
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
getMerchantConfigOnboardingDocument :: ShortId DM.Merchant -> Context.City -> Maybe Common.DocumentType -> Maybe Common.Category -> Flow Common.DocumentVerificationConfigRes
getMerchantConfigOnboardingDocument merchantShortId opCity mbReqDocumentType mbCategory = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  configs <- case (mbReqDocumentType, mbCategory) of
    (Nothing, Nothing) -> CQDVC.findAllByMerchantOpCityId merchantOpCityId
    (Just reqDocumentType, Nothing) -> CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId (castDocumentType reqDocumentType)
    (Nothing, Just category) -> CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId (castCategory category)
    (Just reqDocumentType, Just category) -> maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId (castDocumentType reqDocumentType) (castCategory category)

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
    { vehicleVariant = castDVehicleVariant vehicleVariant,
      vehicleModel = fromMaybe "" vehicleModel,
      ..
    }

castDVehicleVariant :: DVeh.Variant -> Common.Variant
castDVehicleVariant = \case
  DVeh.SUV -> Common.SUV
  DVeh.HATCHBACK -> Common.HATCHBACK
  DVeh.SEDAN -> Common.SEDAN
  DVeh.AUTO_RICKSHAW -> Common.AUTO_RICKSHAW
  DVeh.TAXI -> Common.TAXI
  DVeh.TAXI_PLUS -> Common.TAXI_PLUS
  DVeh.PREMIUM_SEDAN -> Common.PREMIUM_SEDAN
  DVeh.BLACK -> Common.BLACK
  DVeh.BLACK_XL -> Common.BLACK_XL
  DVeh.BIKE -> Common.BIKE
  DVeh.AMBULANCE_TAXI -> Common.AMBULANCE_TAXI
  DVeh.AMBULANCE_TAXI_OXY -> Common.AMBULANCE_TAXI_OXY
  DVeh.AMBULANCE_AC -> Common.AMBULANCE_AC
  DVeh.AMBULANCE_AC_OXY -> Common.AMBULANCE_AC_OXY
  DVeh.AMBULANCE_VENTILATOR -> Common.AMBULANCE_VENTILATOR
  DVeh.SUV_PLUS -> Common.SUV_PLUS

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
  Common.Category ->
  Common.DocumentVerificationConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentUpdate merchantShortId opCity reqDocumentType reqCategory req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  let category = castCategory reqCategory
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  config <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType category >>= fromMaybeM (DocumentVerificationConfigDoesNotExist merchantOpCityId.getId $ show documentType)
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
    { vehicleVariant = castVehicleVariant vehicleVariant,
      vehicleModel = Just vehicleModel,
      ..
    }

castVehicleVariant :: Common.Variant -> DVeh.Variant
castVehicleVariant = \case
  Common.SUV -> DVeh.SUV
  Common.HATCHBACK -> DVeh.HATCHBACK
  Common.SEDAN -> DVeh.SEDAN
  Common.AUTO_RICKSHAW -> DVeh.AUTO_RICKSHAW
  Common.TAXI -> DVeh.TAXI
  Common.TAXI_PLUS -> DVeh.TAXI_PLUS
  Common.PREMIUM_SEDAN -> DVeh.PREMIUM_SEDAN
  Common.BLACK -> DVeh.BLACK
  Common.BLACK_XL -> DVeh.BLACK_XL
  Common.BIKE -> DVeh.BIKE
  Common.AMBULANCE_TAXI -> DVeh.AMBULANCE_TAXI
  Common.AMBULANCE_TAXI_OXY -> DVeh.AMBULANCE_TAXI_OXY
  Common.AMBULANCE_AC -> DVeh.AMBULANCE_AC
  Common.AMBULANCE_AC_OXY -> DVeh.AMBULANCE_AC_OXY
  Common.AMBULANCE_VENTILATOR -> DVeh.AMBULANCE_VENTILATOR
  Common.SUV_PLUS -> DVeh.SUV_PLUS

castVehicleClassCheckType :: Common.VehicleClassCheckType -> DVC.VehicleClassCheckType
castVehicleClassCheckType = \case
  Common.Infix -> DVC.Infix
  Common.Prefix -> DVC.Prefix
  Common.Suffix -> DVC.Suffix

castDocumentType :: Common.DocumentType -> DVC.DocumentType
castDocumentType = \case
  Common.RC -> DVC.VehicleRegistrationCertificate
  Common.DL -> DVC.DriverLicense

castCategory :: Common.Category -> DVeh.Category
castCategory = \case
  Common.CAR -> DVeh.CAR
  Common.MOTORCYCLE -> DVeh.MOTORCYCLE
  Common.TRAIN -> DVeh.TRAIN
  Common.BUS -> DVeh.BUS
  Common.FLIGHT -> DVeh.FLIGHT
  Common.AUTO_CATEGORY -> DVeh.AUTO_CATEGORY
  Common.AMBULANCE -> DVeh.AMBULANCE

---------------------------------------------------------------------
postMerchantConfigOnboardingDocumentCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DocumentType ->
  Common.Category ->
  Common.DocumentVerificationConfigCreateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentCreate merchantShortId opCity reqDocumentType reqCategory req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let documentType = castDocumentType reqDocumentType
  let category = castCategory reqCategory
  mbConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType category
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
        vehicleCategory = DVeh.AUTO_CATEGORY,
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
  config <- CQMSUC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
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
    CQMSUC.findByMerchantOpCityId merchantOpCityId
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
    CQMSUC.findByMerchantOpCityId merchantOpCityId
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
      <> maybe [] Common.getWaitingChargeFields req.waitingCharge
      <> maybe [] Common.getWaitingChargeInfoFields req.waitingChargeInfo
      <> maybe [] Common.getNightShiftChargeFields req.nightShiftCharge
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
          waitingChargeInfo = Common.mkWaitingChargeInfo <$> req.waitingChargeInfo <|> waitingChargeInfo,
          nightShiftCharge = Common.mkNightShiftCharge <$> req.nightShiftCharge <|> nightShiftCharge,
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
    govtCharges :: Text,
    farePolicyType :: Text,
    description :: Text,
    congestionChargeMultiplier :: Text,
    congestionChargeMultiplierIncludeBaseFare :: Text,
    parkingCharge :: Text,
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
    peakTimings :: Text,
    rideDurationSectionStart :: Text,
    perMinuteAmount :: Text,
    perDistanceUnitInsuranceCharge :: Text,
    perDistanceUnitCardChargeMultiplier :: Text,
    fixedCardCharge :: Text,
    peakDays :: Text,
    cancellationFarePolicyDescription :: Text,
    freeCancellationTimeSeconds :: Text,
    maxCancellationCharge :: Text,
    maxWaitingTimeAtPickupSeconds :: Text,
    minCancellationCharge :: Text,
    perMetreCancellationCharge :: Text,
    perMinuteCancellationCharge :: Text,
    percentageOfRideFareToBeCharged :: Text
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
      <*> r .: "govt_charges"
      <*> r .: "fare_policy_type"
      <*> r .: "description"
      <*> r .: "congestion_charge_multiplier"
      <*> r .: "congestion_charge_multiplier_include_base_fare"
      <*> r .: "parking_charge"
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
      <*> r .: "peak_timings"
      <*> r .: "ride_duration_section_start"
      <*> r .: "per_minute_amount"
      <*> r .: "per_distance_unit_insurance_charge"
      <*> r .: "per_distance_unit_card_charge_multiplier"
      <*> r .: "fixed_card_charge"
      <*> r .: "peak_days"
      <*> r .: "cancellation_fare_policy_description"
      <*> r .: "free_cancellation_time_seconds"
      <*> r .: "max_cancellation_charge"
      <*> r .: "max_waiting_time_at_pickup_seconds"
      <*> r .: "min_cancellation_charge"
      <*> r .: "per_metre_cancellation_charge"
      <*> r .: "per_minute_cancellation_charge"
      <*> r .: "percentage_of_ride_fare_to_be_charged"

postMerchantConfigFarePolicyUpsert :: ShortId DM.Merchant -> Context.City -> Common.UpsertFarePolicyReq -> Flow Common.UpsertFarePolicyResp
postMerchantConfigFarePolicyUpsert merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  logTagInfo "Updating Fare Policies for merchant: " (show merchant.id <> " and city: " <> show opCity)
  flatFarePolicies <- readCsv merchantOpCity.distanceUnit req.file
  logTagInfo "Read file: " (show flatFarePolicies)
  let boundedAlreadyDeletedMap = Map.empty :: Map.Map Text Bool
  (farePolicyErrors, _) <- (foldlM (processFarePolicyGroup merchantOpCity) ([], boundedAlreadyDeletedMap) . groupFarePolices) flatFarePolicies
  return $
    Common.UpsertFarePolicyResp
      { unprocessedFarePolicies = farePolicyErrors,
        success = "Fare Policies updated successfully"
      }
  where
    cleanField = replaceEmpty . T.strip

    readCsv distanceUnit csvFile = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector FarePolicyCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM (makeFarePolicy distanceUnit) v >>= (pure . V.toList)

    readCSVField :: Read a => Int -> Text -> Text -> Flow a
    readCSVField idx fieldValue fieldName =
      cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
    readMaybeCSVField _ fieldValue _ =
      cleanField fieldValue >>= readMaybe . T.unpack

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    groupFarePolices :: [(Context.City, DVST.ServiceTierType, DTC.TripCategory, SL.Area, TimeBound, FarePolicy.FarePolicy)] -> [[(Context.City, DVST.ServiceTierType, DTC.TripCategory, SL.Area, TimeBound, FarePolicy.FarePolicy)]]
    groupFarePolices = DL.groupBy (\a b -> fst5 a == fst5 b) . DL.sortBy (compare `on` fst5)
      where
        fst5 (c, t, tr, a, tb, _) = (c, t, tr, a, tb)

    processFarePolicyGroup :: DMOC.MerchantOperatingCity -> ([Text], Map.Map Text Bool) -> [(Context.City, DVST.ServiceTierType, DTC.TripCategory, SL.Area, TimeBound, FarePolicy.FarePolicy)] -> Flow ([Text], Map.Map Text Bool)
    processFarePolicyGroup _ _ [] = throwError $ InvalidRequest "Empty Fare Policy Group"
    processFarePolicyGroup merchantOpCity (errors, boundedAlreadyDeletedMap) (x : xs) = do
      let (city, vehicleServiceTier, tripCategory, area, timeBounds, firstFarePolicy) = x
      if (city /= opCity)
        then return $ (errors <> ["Can't process fare policy for different city: " <> show city <> ", please login with this city in dashboard"], boundedAlreadyDeletedMap)
        else do
          let mergeFarePolicy newId FarePolicy.FarePolicy {..} = do
                let remainingfarePolicies = map (\(_, _, _, _, _, fp) -> fp) xs
                let driverExtraFeeBounds' = NE.nonEmpty $ (maybe [] NE.toList driverExtraFeeBounds) <> concatMap ((maybe [] NE.toList) . (.driverExtraFeeBounds)) remainingfarePolicies
                let driverExtraFeeBoundsDuplicateRemoved = (NE.nubBy (\a b -> a.startDistance == b.startDistance)) <$> driverExtraFeeBounds'
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
                    _ -> return farePolicyDetails
                return $ FarePolicy.FarePolicy {id = newId, driverExtraFeeBounds = driverExtraFeeBoundsDuplicateRemoved, farePolicyDetails = farePolicyDetails', ..}

          newId <- generateGUID
          finalFarePolicy <- mergeFarePolicy newId firstFarePolicy
          CQFP.create finalFarePolicy
          let merchanOperatingCityId = merchantOpCity.id
          (oldFareProducts, newBoundedAlreadyDeletedMap) <-
            case timeBounds of
              Unbounded -> do
                fareProducts <- maybeToList <$> CQFProduct.findUnboundedByMerchantVariantArea merchanOperatingCityId [DFareProduct.ALL] tripCategory vehicleServiceTier area
                return (fareProducts, boundedAlreadyDeletedMap)
              _ -> do
                let key = makeKey merchanOperatingCityId vehicleServiceTier tripCategory area
                let value = Map.lookup key boundedAlreadyDeletedMap
                if isJust value
                  then return ([], boundedAlreadyDeletedMap)
                  else do
                    fareProducts <- CQFProduct.findAllBoundedByMerchantVariantArea merchanOperatingCityId [DFareProduct.ALL] tripCategory vehicleServiceTier area
                    let updatedBoundedAlreadyDeletedMap = markBoundedAreadyDeleted merchanOperatingCityId vehicleServiceTier tripCategory area boundedAlreadyDeletedMap
                    return (fareProducts, updatedBoundedAlreadyDeletedMap)

          oldFareProducts `forM_` \fp -> do
            fareProducts <- CQFProduct.findAllFareProductByFarePolicyId fp.farePolicyId
            when (length fareProducts == 1) $ CQFP.delete fp.farePolicyId
            CQFProduct.delete fp.id
            CQFProduct.clearCache fp

          id <- generateGUID
          let farePolicyId = finalFarePolicy.id
          let fareProduct = DFareProduct.FareProduct {searchSource = DFareProduct.ALL, enabled = True, merchantId = merchantOpCity.merchantId, merchantOperatingCityId = merchantOpCity.id, ..}
          CQFProduct.create fareProduct
          CQFProduct.clearCache fareProduct

          return (errors, newBoundedAlreadyDeletedMap)

    makeFarePolicy :: DistanceUnit -> Int -> FarePolicyCSVRow -> Flow (Context.City, DVST.ServiceTierType, DTC.TripCategory, SL.Area, TimeBound, FarePolicy.FarePolicy)
    makeFarePolicy distanceUnit idx row = do
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
      vehicleServiceTier :: DVST.ServiceTierType <- readCSVField idx row.vehicleServiceTier "Vehicle Service Tier"
      area :: SL.Area <- readCSVField idx row.area "Area"
      idText <- cleanCSVField idx row.farePolicyKey "Fare Policy Key"
      tripCategory :: DTC.TripCategory <- readCSVField idx row.tripCategory "Trip Category"
      nightShiftStart :: TimeOfDay <- readCSVField idx row.nightShiftStart "Night Shift Start"
      nightShiftEnd :: TimeOfDay <- readCSVField idx row.nightShiftEnd "Night Shift End"
      let nightShiftBounds = Just $ Common.NightShiftBounds nightShiftStart nightShiftEnd
      minAllowedTripDistance :: Meters <- readCSVField idx row.minAllowedTripDistance "Min Allowed Trip Distance"
      maxAllowedTripDistance :: Meters <- readCSVField idx row.maxAllowedTripDistance "Max Allowed Trip Distance"
      let allowedTripDistanceBounds = Just $ FarePolicy.AllowedTripDistanceBounds {distanceUnit, minAllowedTripDistance, maxAllowedTripDistance}
      let serviceCharge :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.serviceCharge "Service Charge"
      let tollCharges :: (Maybe HighPrecMoney) = readMaybeCSVField idx row.tollCharges "Toll Charge"
      let perMinuteRideExtraTimeCharge = Nothing
      let govtCharges :: (Maybe Double) = readMaybeCSVField idx row.govtCharges "Govt Charges"
      farePolicyType :: FarePolicy.FarePolicyType <- readCSVField idx row.farePolicyType "Fare Policy Type"
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
      currency :: Currency <- readCSVField idx row.currency "Currency"

      (freeWatingTime, waitingCharges, nightCharges) <- do
        freeWatingTime :: Minutes <- readCSVField idx row.freeWatingTime "Free Waiting Time"
        waitingCharge :: HighPrecMoney <- readCSVField idx row.waitingCharge "Waiting Charge"
        waitingChargeType <- cleanCSVField idx row.waitingChargeType "Waiting Charge Type"
        let waitingCharges =
              case waitingChargeType of
                "PerMinuteWaitingCharge" -> FarePolicy.PerMinuteWaitingCharge waitingCharge
                "ConstantWaitingCharge" -> FarePolicy.ConstantWaitingCharge waitingCharge
                _ -> FarePolicy.PerMinuteWaitingCharge waitingCharge
        nightShiftChargeType <- cleanCSVField idx row.nightShiftChargeType "Night Shift Charge Type"
        nightCharges <-
          case nightShiftChargeType of
            "ProgressiveNightShiftCharge" -> do
              nightShiftCharge :: Float <- readCSVField idx row.nightShiftCharge "Night Shift Charge"
              return $ FarePolicy.ProgressiveNightShiftCharge nightShiftCharge
            "ConstantNightShiftCharge" -> do
              nightShiftCharge :: HighPrecMoney <- readCSVField idx row.nightShiftCharge "Night Shift Charge"
              return $ FarePolicy.ConstantNightShiftCharge nightShiftCharge
            _ -> do
              nightShiftCharge :: Float <- readCSVField idx row.nightShiftCharge "Night Shift Charge"
              return $ FarePolicy.ProgressiveNightShiftCharge nightShiftCharge
        return (freeWatingTime, waitingCharges, nightCharges)

      let perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney = readMaybeCSVField idx row.perDistanceUnitInsuranceCharge "Per Distance Unit Insurance Charge"
          cardCharge =
            Just
              FarePolicy.CardCharge
                { perDistanceUnitMultiplier = readMaybeCSVField idx row.perDistanceUnitCardChargeMultiplier "Per Distance Unit Card Charge Multiplier",
                  fixed = readMaybeCSVField idx row.fixedCardCharge "Fixed Card Charge"
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
            let perExtraKmRateSections = NE.fromList [FarePolicy.FPProgressiveDetailsPerExtraKmRateSection {startDistance, distanceUnit, perExtraKmRate}]

            let mbRideDuration :: Maybe Minutes = readMaybeCSVField idx row.rideDurationSectionStart "Ride Duration Section Start"
            perMinRateSections <-
              forM mbRideDuration $ \rideDuration -> do
                perMinuteAmount :: HighPrecMoney <- readCSVField idx row.perMinuteAmount "Per Minute Amount"
                let perMinRate = mkPrice (Just currency) perMinuteAmount
                pure $ NE.fromList [FarePolicy.FPProgressiveDetailsPerMinRateSection {rideDuration, perMinRate, createdAt = now, updatedAt = now}]
            return $ FarePolicy.ProgressiveDetails FarePolicy.FPProgressiveDetails {nightShiftCharge = Just nightCharges, ..}
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

      return $ (city, vehicleServiceTier, tripCategory, area, timeBound, FarePolicy.FarePolicy {id = Id idText, description = Just description, ..})

    makeKey :: Id DMOC.MerchantOperatingCity -> DVST.ServiceTierType -> DTC.TripCategory -> SL.Area -> Text
    makeKey cityId vehicleServiceTier tripCategory area =
      T.intercalate ":" [cityId.getId, show vehicleServiceTier, show area, show tripCategory]

    markBoundedAreadyDeleted :: Id DMOC.MerchantOperatingCity -> DVST.ServiceTierType -> DTC.TripCategory -> SL.Area -> Map.Map Text Bool -> Map.Map Text Bool
    markBoundedAreadyDeleted cityId vehicleServiceTier tripCategory area mapObj = do
      let key = makeKey cityId vehicleServiceTier tripCategory area
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
      merchantOperatingCity <- maybe (return Nothing) (CQMOC.findByMerchantShortIdAndCity merchantShortId) request.city
      locationName <-
        fromMaybeM (InvalidRequest "Location Name cannot be empty for a new special location") $
          request.locationName <|> (mbExistingSpLoc <&> (.locationName))
      category <- fromMaybeM (InvalidRequest "Category is a required field for a new special location") $ request.category <|> (mbExistingSpLoc <&> (.category))
      return $
        SL.SpecialLocation
          { gates = [],
            createdAt = maybe now (.createdAt) mbExistingSpLoc,
            updatedAt = now,
            merchantOperatingCityId = (.id.getId) <$> merchantOperatingCity,
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
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  createOrUpdateGate existingGates request
  return Success
  where
    createOrUpdateGate :: [(D.GateInfo, Maybe Text)] -> Common.UpsertSpecialLocationGateReqT -> Flow ()
    createOrUpdateGate existingGates req = do
      let existingGateWithGeom = find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName req.name) existingGates
          existingGate = fst <$> existingGateWithGeom
          mbGeom = snd =<< existingGateWithGeom
      updatedGate <- mkGate req existingGate mbGeom
      void $
        runTransaction $
          if isNothing existingGate then QGIG.create updatedGate else QGIG.updateGate updatedGate

    mkGate :: Common.UpsertSpecialLocationGateReqT -> Maybe D.GateInfo -> Maybe Text -> Flow D.GateInfo
    mkGate reqT mbGate mbGeom = do
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
  merchant <- findMerchantByShortId merchantShortId
  baseOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)

  -- city
  baseOperatingCity <- CQMOC.findById baseOperatingCityId >>= fromMaybeM (InvalidRequest "Base Operating City not found")
  newOperatingCity <- buildMerchantOperatingCity merchant.id baseOperatingCity

  -- intelligent pool config
  intelligentPoolConfig <- CDIPC.findByMerchantOpCityId baseOperatingCityId Nothing >>= fromMaybeM (InvalidRequest "Intelligent Pool Config not found")
  newIntelligentPoolConfig <- buildIntelligentPoolConfig newOperatingCity.id intelligentPoolConfig

  -- driver pool config
  driverPoolConfigs <- CQDPC.findAllByMerchantOpCityId baseOperatingCityId
  newDriverPoolConfigs <- mapM (buildPoolConfig newOperatingCity.id) driverPoolConfigs

  -- fare products
  fareProducts <- CQFProduct.findAllFareProductByMerchantOpCityId baseOperatingCityId
  newFareProducts <- mapM (buildFareProduct newOperatingCity.id) fareProducts

  -- vehicle service tier
  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId baseOperatingCityId
  newVehicleServiceTiers <- mapM (buildVehicleServiceTier newOperatingCity.id) vehicleServiceTiers

  -- go home config
  goHomeConfig <- CGHC.findByMerchantOpCityId baseOperatingCityId Nothing
  newGoHomeConfig <- buildGoHomeConfig newOperatingCity.id goHomeConfig

  -- leader board configs
  leaderBoardConfigs <- CQLBC.findAllByMerchantOpCityId baseOperatingCityId
  newLeaderBoardConfigs <- mapM (buildLeaderBoardConfig newOperatingCity.id) leaderBoardConfigs

  -- merchant message
  merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId
  newMerchantMessages <- mapM (buildMerchantMessage newOperatingCity.id) merchantMessages

  -- merchant overlay
  merchantOverlays <- CQMO.findAllByMerchantOpCityId baseOperatingCityId
  newMerchantOverlays <- mapM (buildMerchantOverlay newOperatingCity.id) merchantOverlays

  -- merchant payment method
  merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId baseOperatingCityId
  newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newOperatingCity.id) merchantPaymentMethods

  -- merchant service usage config
  merchantServiceUsageConfig <- CQMSUC.findByMerchantOpCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
  newMerchantServiceUsageConfig <- buildMerchantServiceUsageConfig newOperatingCity.id merchantServiceUsageConfig

  -- merchant service config
  merchantServiceConfigs <- CQMSC.findAllMerchantOpCityId baseOperatingCityId
  newMerchantServiceConfigs <- mapM (buildMerchantServiceConfig newOperatingCity.id) merchantServiceConfigs

  -- onboarding document config
  documentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId baseOperatingCityId
  newDocumentVerificationConfigs <- mapM (buildNewDocumentVerificationConfig newOperatingCity.id) documentVerificationConfigs

  -- transporter config
  transporterConfig <- CTC.findByMerchantOpCityId baseOperatingCityId Nothing >>= fromMaybeM (InvalidRequest "Transporter Config not found")
  newTransporterConfig <- buildTransporterConfig newOperatingCity.id transporterConfig

  -- geometry
  geometry <- buildGeometry

  -- call ride exophone
  exophones <- CQExophone.findAllCallExophoneByMerchantOpCityId baseOperatingCityId

  QGEO.create geometry
  CQMOC.create newOperatingCity
  CQDIPC.create newIntelligentPoolConfig
  mapM_ CQDPC.create newDriverPoolConfigs
  mapM_ CQFProduct.create newFareProducts
  CQVST.createMany newVehicleServiceTiers
  CQGHC.create newGoHomeConfig
  mapM_ CQLBC.create newLeaderBoardConfigs
  mapM_ CQMM.create newMerchantMessages
  mapM_ CQMO.create newMerchantOverlays
  mapM_ CQMPM.create newMerchantPaymentMethods
  CQMSUC.create newMerchantServiceUsageConfig
  mapM_ CQMSC.create newMerchantServiceConfigs
  mapM_ CQDVC.create newDocumentVerificationConfigs
  CQTC.create newTransporterConfig
  whenJust (find (\exophone -> exophone.exophoneType == DExophone.CALL_RIDE) exophones) $ \exophone -> do
    exophone' <- buildNewExophone newOperatingCity.id exophone
    CQExophone.create exophone'

  when req.enableForMerchant $ do
    let newOrigin = updateGeoRestriction merchant.geofencingConfig.origin
        newDestination = updateGeoRestriction merchant.geofencingConfig.destination
    CQM.updateGeofencingConfig merchant.id newOrigin newDestination
    CQM.clearCache merchant

  pure $ Common.CreateMerchantOperatingCityRes newOperatingCity.id.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [(show req.city)]

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

    buildMerchantOperatingCity merchantId baseCity = do
      id <- generateGUID
      pure
        DMOC.MerchantOperatingCity
          { id,
            merchantId,
            merchantShortId,
            location = Maps.LatLong req.lat req.long,
            city = req.city,
            state = req.state,
            country = req.country,
            supportNumber = req.supportNumber <|> baseCity.supportNumber,
            language = fromMaybe baseCity.language req.primaryLanguage,
            currency = fromMaybe baseCity.currency req.currency,
            distanceUnit = fromMaybe baseCity.distanceUnit req.distanceUnit
          }

    buildIntelligentPoolConfig newCityId DDIPC.DriverIntelligentPoolConfig {..} = do
      now <- getCurrentTime
      return $
        DDIPC.DriverIntelligentPoolConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildPoolConfig newCityId DDPC.DriverPoolConfig {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DDPC.DriverPoolConfig
          { id = newId,
            merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildNewExophone newCityId DExophone.Exophone {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DExophone.Exophone
          { id = newId,
            merchantOperatingCityId = newCityId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildFareProduct newCityId DFareProduct.FareProduct {..} = do
      newId <- generateGUID
      return $
        DFareProduct.FareProduct
          { id = newId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildVehicleServiceTier newCityId DVST.VehicleServiceTier {..} = do
      newId <- generateGUID
      return $
        DVST.VehicleServiceTier
          { id = newId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildGoHomeConfig newCityId DGoHomeConfig.GoHomeConfig {..} = do
      now <- getCurrentTime
      return $
        DGoHomeConfig.GoHomeConfig
          { merchantOperatingCityId = newCityId,
            enableGoHome = False,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildLeaderBoardConfig newCityId DLC.LeaderBoardConfigs {..} = do
      newId <- generateGUID
      return $
        DLC.LeaderBoardConfigs
          { id = newId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildMerchantMessage newCityId DMM.MerchantMessage {..} = do
      now <- getCurrentTime
      return $
        DMM.MerchantMessage
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantOverlay newCityId DMO.Overlay {..} = do
      newId <- generateGUID
      return $
        DMO.Overlay
          { id = newId,
            merchantOperatingCityId = newCityId,
            ..
          }

    buildMerchantPaymentMethod newCityId DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantServiceUsageConfig newCityId DMSUC.MerchantServiceUsageConfig {..} = do
      now <- getCurrentTime
      return $
        DMSUC.MerchantServiceUsageConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantServiceConfig newCityId DMSC.MerchantServiceConfig {..} = do
      now <- getCurrentTime
      return $
        DMSC.MerchantServiceConfig
          { merchantOperatingCityId = Just newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildNewDocumentVerificationConfig newCityId DVC.DocumentVerificationConfig {..} = do
      now <- getCurrentTime
      return $
        DVC.DocumentVerificationConfig
          { merchantOperatingCityId = newCityId,
            rcNumberPrefixList = fromMaybe rcNumberPrefixList req.rcNumberPrefixList,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildTransporterConfig newCityId DTC.TransporterConfig {..} = do
      now <- getCurrentTime
      return $
        DTC.TransporterConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
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
