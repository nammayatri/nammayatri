{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequestForDriver where

import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.SearchRequestForDriver as Beam
import Storage.Queries.Transformers.SearchRequestForDriver
import qualified Storage.Queries.Transformers.SearchRequestForDriver

instance FromTType' Beam.SearchRequestForDriver Domain.Types.SearchRequestForDriver.SearchRequestForDriver where
  fromTType' (Beam.SearchRequestForDriverT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequestForDriver.getMerchantOpCId merchantOperatingCityId merchantId requestId
    pure $
      Just
        Domain.Types.SearchRequestForDriver.SearchRequestForDriver
          { acceptanceRatio = acceptanceRatio,
            actualDistanceToPickup = actualDistanceToPickup,
            airConditioned = airConditioned,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount <$> baseFare,
            batchNumber = batchNumber,
            cancellationRatio = cancellationRatio,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientSdkVersion = clientSdkVersion',
            coinsRewardedOnGoldTierRide = coinsRewardedOnGoldTierRide,
            commissionCharges = commissionCharges,
            conditionalCharges = Kernel.Prelude.fromMaybe [] $ Kernel.Prelude.readMaybe . Data.Text.unpack =<< conditionalCharges,
            createdAt = Data.Time.localTimeToUTC Data.Time.utc createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            customerCancellationDues = getCustomerCancellationDues customerCancellationDues,
            customerTags = customerTags,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverAvailableTime = driverAvailableTime,
            driverDefaultStepFee = Kernel.Types.Common.mkAmountWithDefault driverDefaultStepFeeAmount <$> driverDefaultStepFee,
            driverId = Kernel.Types.Id.Id driverId,
            driverMaxExtraFee = Kernel.Types.Common.mkAmountWithDefault driverMaxExtraFeeAmount <$> driverMaxExtraFee,
            driverMinExtraFee = Kernel.Types.Common.mkAmountWithDefault driverMinExtraFeeAmount <$> driverMinExtraFee,
            driverSpeed = driverSpeed,
            driverStepFee = Kernel.Types.Common.mkAmountWithDefault driverStepFeeAmount <$> driverStepFee,
            driverTagScore = driverTagScore,
            driverTags = driverTags,
            durationToPickup = durationToPickup,
            estimateId = estimateId,
            fromLocGeohash = fromLocGeohash,
            goHomeRequestId = Kernel.Types.Id.Id <$> goHomeRequestId,
            id = Kernel.Types.Id.Id id,
            isFavourite = isFavourite,
            isForwardRequest = Kernel.Prelude.fromMaybe False isForwardRequest,
            isPartOfIntelligentPool = isPartOfIntelligentPool,
            isSafetyPlus = isSafetyPlus,
            keepHiddenForSeconds = keepHiddenForSeconds,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            middleStopCount = middleStopCount,
            mode = mode,
            notificationSource = notificationSource,
            parallelSearchRequestCount = parallelSearchRequestCount,
            parcelQuantity = parcelQuantity,
            parcelType = parcelType,
            pickupZone = pickupZone,
            poolingConfigVersion = poolingConfigVersion,
            poolingLogicVersion = poolingLogicVersion,
            previousDropGeoHash = previousDropGeoHash,
            reactBundleVersion = reactBundleVersion,
            renderedAt = renderedAt,
            requestId = Kernel.Types.Id.Id requestId,
            respondedAt = respondedAt,
            response = response,
            rideFrequencyScore = rideFrequencyScore,
            rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
            searchRequestValidTill = Data.Time.localTimeToUTC Data.Time.utc searchRequestValidTill,
            searchTryId = Kernel.Types.Id.Id searchTryId,
            startTime = startTime,
            status = status,
            straightLineDistanceToPickup = straightLineDistanceToPickup,
            totalRides = Kernel.Prelude.fromMaybe 0 totalRides,
            tripEstimatedDistance = tripEstimatedDistance,
            tripEstimatedDuration = tripEstimatedDuration,
            updatedAt = updatedAt,
            upgradeCabRequest = upgradeCabRequest,
            vehicleAge = vehicleAge,
            vehicleCategory = vehicleCategory,
            vehicleServiceTier = Kernel.Prelude.fromMaybe (Domain.Types.VehicleVariant.castVariantToServiceTier vehicleVariant) vehicleServiceTier,
            vehicleServiceTierName = vehicleServiceTierName,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.SearchRequestForDriver Domain.Types.SearchRequestForDriver.SearchRequestForDriver where
  toTType' (Domain.Types.SearchRequestForDriver.SearchRequestForDriver {..}) = do
    Beam.SearchRequestForDriverT
      { Beam.acceptanceRatio = acceptanceRatio,
        Beam.actualDistanceToPickup = actualDistanceToPickup,
        Beam.airConditioned = airConditioned,
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.baseFare = Kernel.Prelude.roundToIntegral <$> baseFare,
        Beam.baseFareAmount = baseFare,
        Beam.batchNumber = batchNumber,
        Beam.cancellationRatio = cancellationRatio,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.coinsRewardedOnGoldTierRide = coinsRewardedOnGoldTierRide,
        Beam.commissionCharges = commissionCharges,
        Beam.conditionalCharges = Kernel.Prelude.Just $ Kernel.Prelude.show conditionalCharges,
        Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.customerCancellationDues = Kernel.Prelude.Just customerCancellationDues,
        Beam.customerTags = customerTags,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverAvailableTime = driverAvailableTime,
        Beam.driverDefaultStepFee = Kernel.Prelude.roundToIntegral <$> driverDefaultStepFee,
        Beam.driverDefaultStepFeeAmount = driverDefaultStepFee,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverMaxExtraFee = Kernel.Prelude.roundToIntegral <$> driverMaxExtraFee,
        Beam.driverMaxExtraFeeAmount = driverMaxExtraFee,
        Beam.driverMinExtraFee = Kernel.Prelude.roundToIntegral <$> driverMinExtraFee,
        Beam.driverMinExtraFeeAmount = driverMinExtraFee,
        Beam.driverSpeed = driverSpeed,
        Beam.driverStepFee = Kernel.Prelude.roundToIntegral <$> driverStepFee,
        Beam.driverStepFeeAmount = driverStepFee,
        Beam.driverTagScore = driverTagScore,
        Beam.driverTags = driverTags,
        Beam.durationToPickup = durationToPickup,
        Beam.estimateId = estimateId,
        Beam.fromLocGeohash = fromLocGeohash,
        Beam.goHomeRequestId = Kernel.Types.Id.getId <$> goHomeRequestId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isFavourite = isFavourite,
        Beam.isForwardRequest = Kernel.Prelude.Just isForwardRequest,
        Beam.isPartOfIntelligentPool = isPartOfIntelligentPool,
        Beam.isSafetyPlus = isSafetyPlus,
        Beam.keepHiddenForSeconds = keepHiddenForSeconds,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.middleStopCount = middleStopCount,
        Beam.mode = mode,
        Beam.notificationSource = notificationSource,
        Beam.parallelSearchRequestCount = parallelSearchRequestCount,
        Beam.parcelQuantity = parcelQuantity,
        Beam.parcelType = parcelType,
        Beam.pickupZone = pickupZone,
        Beam.poolingConfigVersion = poolingConfigVersion,
        Beam.poolingLogicVersion = poolingLogicVersion,
        Beam.previousDropGeoHash = previousDropGeoHash,
        Beam.reactBundleVersion = reactBundleVersion,
        Beam.renderedAt = renderedAt,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.respondedAt = respondedAt,
        Beam.response = response,
        Beam.rideFrequencyScore = rideFrequencyScore,
        Beam.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
        Beam.searchRequestValidTill = Data.Time.utcToLocalTime Data.Time.utc searchRequestValidTill,
        Beam.searchTryId = Kernel.Types.Id.getId searchTryId,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.straightLineDistanceToPickup = straightLineDistanceToPickup,
        Beam.totalRides = Kernel.Prelude.Just totalRides,
        Beam.tripEstimatedDistance = tripEstimatedDistance,
        Beam.tripEstimatedDuration = tripEstimatedDuration,
        Beam.updatedAt = updatedAt,
        Beam.upgradeCabRequest = upgradeCabRequest,
        Beam.vehicleAge = vehicleAge,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleServiceTier = Kernel.Prelude.Just vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName,
        Beam.vehicleVariant = vehicleVariant
      }
