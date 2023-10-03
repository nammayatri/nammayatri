{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( mapsServiceConfigUpdate,
    mapsServiceUsageConfigUpdate,
    merchantCommonConfig,
    merchantCommonConfigUpdate,
    driverPoolConfig,
    driverPoolConfigUpdate,
    driverPoolConfigCreate,
    driverIntelligentPoolConfig,
    driverIntelligentPoolConfigUpdate,
    onboardingDocumentConfig,
    onboardingDocumentConfigUpdate,
    onboardingDocumentConfigCreate,
    merchantUpdate,
    serviceUsageConfig,
    smsServiceConfigUpdate,
    smsServiceUsageConfigUpdate,
    verificationServiceConfigUpdate,
    createFPDriverExtraFee,
    updateFPDriverExtraFee,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FarePolicy as FarePolicy
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DFPEFB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as DDIPC
import qualified Domain.Types.Merchant.DriverPoolConfig as DDPC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as DODC
import qualified Domain.Types.Merchant.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import Environment
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as DriverPool
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.FarePolicy as CQFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as CQDIPC
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CQDPC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.OnboardingDocumentConfig as CQODC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QFPEFB
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow Common.MerchantUpdateRes
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.description = req.description <|> merchant.description,
                 DM.enabled = fromMaybe merchant.enabled req.enabled
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantId /= merchant.id) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  _ <- CQM.update updMerchant
  whenJust req.exoPhones \exophones -> do
    CQExophone.deleteByMerchantId merchant.id
    forM_ exophones $ \exophoneReq -> do
      exophone <- buildExophone merchant.id now exophoneReq
      CQExophone.create exophone
  whenJust req.fcmConfig $
    \fcmConfig -> CQTC.updateFCMConfig merchant.id fcmConfig.fcmUrl fcmConfig.fcmServiceAccount

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantId == merchant.id) allExophones
    CQExophone.clearCache merchant.id oldExophones
  whenJust req.fcmConfig $ \_ -> CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  return $ mkMerchantUpdateRes updMerchant
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
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
merchantCommonConfig :: ShortId DM.Merchant -> Flow Common.MerchantCommonConfigRes
merchantCommonConfig merchantShortId = do
  merchant <- findMerchantByShortId merchantShortId
  config <- CQTC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
  pure $ mkMerchantCommonConfigRes config

mkMerchantCommonConfigRes :: DTC.TransporterConfig -> Common.MerchantCommonConfigRes
mkMerchantCommonConfigRes DTC.TransporterConfig {..} = Common.MerchantCommonConfigRes {..}

---------------------------------------------------------------------
merchantCommonConfigUpdate :: ShortId DM.Merchant -> Common.MerchantCommonConfigUpdateReq -> Flow APISuccess
merchantCommonConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQTC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
  let updConfig =
        config{pickupLocThreshold = maybe config.pickupLocThreshold (.value) req.pickupLocThreshold,
               dropLocThreshold = maybe config.dropLocThreshold (.value) req.dropLocThreshold,
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
               driverFeeCalculatorBatchGap = maybe config.driverFeeCalculatorBatchGap (.value) req.driverFeeCalculatorBatchGap
              }
  _ <- CQTC.update updConfig
  CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantCommonConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
driverPoolConfig :: ShortId DM.Merchant -> Maybe Meters -> Flow Common.DriverPoolConfigRes
driverPoolConfig merchantShortId mbTripDistance = do
  merchant <- findMerchantByShortId merchantShortId
  configs <- case mbTripDistance of
    Nothing -> CQDPC.findAllByMerchantId merchant.id
    Just tripDistance -> maybeToList <$> CQDPC.findByMerchantIdAndTripDistance merchant.id tripDistance
  pure $ mkDriverPoolConfigRes <$> configs

mkDriverPoolConfigRes :: DDPC.DriverPoolConfig -> Common.DriverPoolConfigItem
mkDriverPoolConfigRes DDPC.DriverPoolConfig {..} =
  Common.DriverPoolConfigItem
    { poolSortingType = castDPoolSortingType poolSortingType,
      ..
    }

castDPoolSortingType :: DriverPool.PoolSortingType -> Common.PoolSortingType
castDPoolSortingType = \case
  DriverPool.Intelligent -> Common.Intelligent
  DriverPool.Random -> Common.Random

---------------------------------------------------------------------
driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  Flow APISuccess
driverPoolConfigUpdate merchantShortId tripDistance req = do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQDPC.findByMerchantIdAndTripDistance merchant.id tripDistance >>= fromMaybeM (DriverPoolConfigDoesNotExist merchant.id.getId tripDistance)
  let updConfig =
        config{minRadiusOfSearch = maybe config.minRadiusOfSearch (.value) req.minRadiusOfSearch,
               maxRadiusOfSearch = maybe config.maxRadiusOfSearch (.value) req.maxRadiusOfSearch,
               radiusStepSize = maybe config.radiusStepSize (.value) req.radiusStepSize,
               driverPositionInfoExpiry = maybe config.driverPositionInfoExpiry (.value) req.driverPositionInfoExpiry,
               actualDistanceThreshold = maybe config.actualDistanceThreshold (.value) req.actualDistanceThreshold,
               maxDriverQuotesRequired = maybe config.maxDriverQuotesRequired (.value) req.maxDriverQuotesRequired,
               driverQuoteLimit = maybe config.driverQuoteLimit (.value) req.driverQuoteLimit,
               driverRequestCountLimit = maybe config.driverRequestCountLimit (.value) req.driverRequestCountLimit,
               driverBatchSize = maybe config.driverBatchSize (.value) req.driverBatchSize,
               maxNumberOfBatches = maybe config.maxNumberOfBatches (.value) req.maxNumberOfBatches,
               maxParallelSearchRequests = maybe config.maxParallelSearchRequests (.value) req.maxParallelSearchRequests,
               poolSortingType = maybe config.poolSortingType (castPoolSortingType . (.value)) req.poolSortingType,
               singleBatchProcessTime = maybe config.singleBatchProcessTime (.value) req.singleBatchProcessTime,
               distanceBasedBatchSplit = maybe config.distanceBasedBatchSplit (map castBatchSplitByPickupDistance . (.value)) req.distanceBasedBatchSplit
              }
  _ <- CQDPC.update updConfig
  CQDPC.clearCache merchant.id
  logTagInfo "dashboard -> driverPoolConfigUpdate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

castPoolSortingType :: Common.PoolSortingType -> DriverPool.PoolSortingType
castPoolSortingType = \case
  Common.Intelligent -> DriverPool.Intelligent
  Common.Random -> DriverPool.Random

castBatchSplitByPickupDistance :: Common.BatchSplitByPickupDistance -> DriverPool.BatchSplitByPickupDistance
castBatchSplitByPickupDistance Common.BatchSplitByPickupDistance {..} = DriverPool.BatchSplitByPickupDistance {..}

---------------------------------------------------------------------
driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  Flow APISuccess
driverPoolConfigCreate merchantShortId tripDistance req = do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  mbConfig <- CQDPC.findByMerchantIdAndTripDistance merchant.id tripDistance
  whenJust mbConfig $ \_ -> throwError (DriverPoolConfigAlreadyExists merchant.id.getId tripDistance)
  newConfig <- buildDriverPoolConfig merchant.id tripDistance req
  _ <- CQDPC.create newConfig
  -- We should clear cache here, because cache contains list of all configs for current merchantId
  CQDPC.clearCache merchant.id
  logTagInfo "dashboard -> driverPoolConfigCreate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

buildDriverPoolConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  m DDPC.DriverPoolConfig
buildDriverPoolConfig merchantId tripDistance Common.DriverPoolConfigCreateReq {..} = do
  now <- getCurrentTime
  pure
    DDPC.DriverPoolConfig
      { merchantId,
        poolSortingType = castPoolSortingType poolSortingType,
        distanceBasedBatchSplit = map castBatchSplitByPickupDistance distanceBasedBatchSplit,
        updatedAt = now,
        createdAt = now,
        ..
      }

---------------------------------------------------------------------
driverIntelligentPoolConfig :: ShortId DM.Merchant -> Flow Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig merchantShortId = do
  merchant <- findMerchantByShortId merchantShortId
  config <- CQDIPC.findByMerchantId merchant.id >>= fromMaybeM (DriverIntelligentPoolConfigNotFound merchant.id.getId)
  pure $ mkDriverIntelligentPoolConfigRes config

mkDriverIntelligentPoolConfigRes :: DDIPC.DriverIntelligentPoolConfig -> Common.DriverIntelligentPoolConfigRes
mkDriverIntelligentPoolConfigRes DDIPC.DriverIntelligentPoolConfig {..} = Common.DriverIntelligentPoolConfigRes {..}

---------------------------------------------------------------------
driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  Flow APISuccess
driverIntelligentPoolConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQDIPC.findByMerchantId merchant.id >>= fromMaybeM (DriverIntelligentPoolConfigNotFound merchant.id.getId)
  let updConfig =
        config{availabilityTimeWeightage = maybe config.availabilityTimeWeightage (.value) req.availabilityTimeWeightage,
               availabilityTimeWindowOption = fromMaybe config.availabilityTimeWindowOption req.availabilityTimeWindowOption,
               acceptanceRatioWeightage = maybe config.acceptanceRatioWeightage (.value) req.acceptanceRatioWeightage,
               acceptanceRatioWindowOption = fromMaybe config.acceptanceRatioWindowOption req.acceptanceRatioWindowOption,
               cancellationRatioWeightage = maybe config.cancellationRatioWeightage (.value) req.cancellationRatioWeightage,
               cancellationRatioWindowOption = fromMaybe config.cancellationRatioWindowOption req.cancellationRatioWindowOption,
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
  CQDIPC.clearCache merchant.id
  logTagInfo "dashboard -> driverIntelligentPoolConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
onboardingDocumentConfig :: ShortId DM.Merchant -> Maybe Common.DocumentType -> Flow Common.OnboardingDocumentConfigRes
onboardingDocumentConfig merchantShortId mbReqDocumentType = do
  merchant <- findMerchantByShortId merchantShortId
  configs <- case mbReqDocumentType of
    Nothing -> CQODC.findAllByMerchantId merchant.id
    Just reqDocumentType -> maybeToList <$> CQODC.findByMerchantIdAndDocumentType merchant.id (castDocumentType reqDocumentType)
  pure $ mkOnboardingDocumentConfigRes <$> configs

mkOnboardingDocumentConfigRes :: DODC.OnboardingDocumentConfig -> Common.OnboardingDocumentConfigItem
mkOnboardingDocumentConfigRes DODC.OnboardingDocumentConfig {..} =
  Common.OnboardingDocumentConfigItem
    { documentType = castDDocumentType documentType,
      vehicleClassCheckType = castDVehicleClassCheckType vehicleClassCheckType,
      supportedVehicleClasses = castDSupportedVehicleClasses supportedVehicleClasses,
      ..
    }

castDSupportedVehicleClasses :: DODC.SupportedVehicleClasses -> Common.SupportedVehicleClasses
castDSupportedVehicleClasses = \case
  DODC.DLValidClasses cfg -> Common.DLValidClasses cfg
  DODC.RCValidClasses cfg -> Common.RCValidClasses (castDClassVariantMap <$> cfg)

castDClassVariantMap :: DODC.VehicleClassVariantMap -> Common.VehicleClassVariantMap
castDClassVariantMap DODC.VehicleClassVariantMap {..} =
  Common.VehicleClassVariantMap
    { vehicleVariant = castDVehicleVariant vehicleVariant,
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
  DVeh.BUS -> Common.BUS

castDVehicleClassCheckType :: DODC.VehicleClassCheckType -> Common.VehicleClassCheckType
castDVehicleClassCheckType = \case
  DODC.Infix -> Common.Infix
  DODC.Prefix -> Common.Prefix
  DODC.Suffix -> Common.Suffix

castDDocumentType :: DODC.DocumentType -> Common.DocumentType
castDDocumentType = \case
  DODC.RC -> Common.RC
  DODC.DL -> Common.DL
  DODC.RCInsurance -> Common.RCInsurance

---------------------------------------------------------------------
onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  Flow APISuccess
onboardingDocumentConfigUpdate merchantShortId reqDocumentType req = do
  -- runRequestValidation Common.validateOnboardingDocumentConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  config <- CQODC.findByMerchantIdAndDocumentType merchant.id documentType >>= fromMaybeM (OnboardingDocumentConfigDoesNotExist merchant.id.getId $ show documentType)
  let updConfig =
        config{checkExtraction = maybe config.checkExtraction (.value) req.checkExtraction,
               checkExpiry = maybe config.checkExpiry (.value) req.checkExpiry,
               supportedVehicleClasses = maybe config.supportedVehicleClasses castSupportedVehicleClasses req.supportedVehicleClasses,
               vehicleClassCheckType = maybe config.vehicleClassCheckType (castVehicleClassCheckType . (.value)) req.vehicleClassCheckType,
               rcNumberPrefix = maybe config.rcNumberPrefix (.value) req.rcNumberPrefix
              }
  _ <- CQODC.update updConfig
  CQODC.clearCache merchant.id
  logTagInfo "dashboard -> onboardingDocumentConfigUpdate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

castSupportedVehicleClasses :: Common.SupportedVehicleClasses -> DODC.SupportedVehicleClasses
castSupportedVehicleClasses = \case
  Common.DLValidClasses cfg -> DODC.DLValidClasses cfg
  Common.RCValidClasses cfg -> DODC.RCValidClasses (castClassVariantMap <$> cfg)

castClassVariantMap :: Common.VehicleClassVariantMap -> DODC.VehicleClassVariantMap
castClassVariantMap Common.VehicleClassVariantMap {..} =
  DODC.VehicleClassVariantMap
    { vehicleVariant = castVehicleVariant vehicleVariant,
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
  Common.BUS -> DVeh.BUS

castVehicleClassCheckType :: Common.VehicleClassCheckType -> DODC.VehicleClassCheckType
castVehicleClassCheckType = \case
  Common.Infix -> DODC.Infix
  Common.Prefix -> DODC.Prefix
  Common.Suffix -> DODC.Suffix

castDocumentType :: Common.DocumentType -> DODC.DocumentType
castDocumentType = \case
  Common.RC -> DODC.RC
  Common.DL -> DODC.DL
  Common.RCInsurance -> DODC.RCInsurance

---------------------------------------------------------------------
onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  Flow APISuccess
onboardingDocumentConfigCreate merchantShortId reqDocumentType req = do
  -- runRequestValidation Common.validateOnboardingDocumentConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  mbConfig <- CQODC.findByMerchantIdAndDocumentType merchant.id documentType
  whenJust mbConfig $ \_ -> throwError (OnboardingDocumentConfigAlreadyExists merchant.id.getId $ show documentType)
  newConfig <- buildOnboardingDocumentConfig merchant.id documentType req
  _ <- CQODC.create newConfig
  -- We should clear cache here, because cache contains list of all configs for current merchantId
  CQODC.clearCache merchant.id
  logTagInfo "dashboard -> onboardingDocumentConfigCreate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

buildOnboardingDocumentConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  DODC.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  m DODC.OnboardingDocumentConfig
buildOnboardingDocumentConfig merchantId documentType Common.OnboardingDocumentConfigCreateReq {..} = do
  now <- getCurrentTime
  pure
    DODC.OnboardingDocumentConfig
      { merchantId,
        vehicleClassCheckType = castVehicleClassCheckType vehicleClassCheckType,
        supportedVehicleClasses = castSupportedVehicleClasses supportedVehicleClasses,
        updatedAt = now,
        createdAt = now,
        ..
      }

---------------------------------------------------------------------
mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
mapsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> mapsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
smsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> smsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
serviceUsageConfig ::
  ShortId DM.Merchant ->
  Flow Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId = do
  merchant <- findMerchantByShortId merchantShortId
  config <- CQMSUC.findByMerchantId merchant.id >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
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
mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
mapsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
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
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> mapsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
smsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> smsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.VerificationServiceConfigUpdateReq ->
  Flow APISuccess
verificationServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.VerificationService $ Common.getVerificationServiceFromReq req
  serviceConfig <- DMSC.VerificationServiceConfig <$> Common.buildVerificationServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> verificationServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------

createFPDriverExtraFee :: ShortId DM.Merchant -> Id FarePolicy.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
createFPDriverExtraFee _ farePolicyId startDistance req = do
  mbFarePolicy <- QFPEFB.findByFarePolicyIdAndStartDistance farePolicyId startDistance
  whenJust mbFarePolicy $ \_ -> throwError $ InvalidRequest "Fare policy with the same id and startDistance already exists"
  farePolicyDetails <- buildFarePolicy farePolicyId startDistance req
  _ <- QFPEFB.create farePolicyDetails
  CQFP.clearCacheById farePolicyId
  pure Success
  where
    buildFarePolicy fpId strtDistance request = do
      let driverExtraFeeBounds =
            DFPEFB.DriverExtraFeeBounds
              { startDistance = strtDistance,
                minFee = request.minFee,
                maxFee = request.maxFee
              }
      return (fpId, driverExtraFeeBounds)

---------------------------------------------------------------------
updateFPDriverExtraFee :: ShortId DM.Merchant -> Id FarePolicy.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
updateFPDriverExtraFee _ farePolicyId startDistance req = do
  _ <- QFPEFB.findByFarePolicyIdAndStartDistance farePolicyId startDistance >>= fromMaybeM (InvalidRequest "Fare Policy with given id and startDistance not found")
  _ <- QFPEFB.update farePolicyId startDistance req.minFee req.maxFee
  CQFP.clearCacheById farePolicyId
  pure Success
