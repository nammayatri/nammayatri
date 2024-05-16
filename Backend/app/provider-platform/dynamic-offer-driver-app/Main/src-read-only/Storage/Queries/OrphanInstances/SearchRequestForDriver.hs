{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequestForDriver where

import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.SearchRequestForDriver
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified SharedLogic.DriverPool.Types
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
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientSdkVersion = clientSdkVersion',
            createdAt = Data.Time.localTimeToUTC Data.Time.utc searchRequestValidTill,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            customerCancellationDues = getCustomerCancellationDues customerCancellationDues,
            driverAvailableTime = driverAvailableTime,
            driverDefaultStepFee = Kernel.Types.Common.mkAmountWithDefault driverDefaultStepFeeAmount <$> driverDefaultStepFee,
            driverId = Kernel.Types.Id.Id driverId,
            driverMaxExtraFee = Kernel.Types.Common.mkAmountWithDefault driverMaxExtraFeeAmount <$> driverMaxExtraFee,
            driverMinExtraFee = Kernel.Types.Common.mkAmountWithDefault driverMinExtraFeeAmount <$> driverMinExtraFee,
            driverSpeed = driverSpeed,
            driverStepFee = Kernel.Types.Common.mkAmountWithDefault driverStepFeeAmount <$> driverStepFee,
            durationToPickup = durationToPickup,
            estimateId = estimateId,
            goHomeRequestId = Kernel.Types.Id.Id <$> goHomeRequestId,
            id = Kernel.Types.Id.Id id,
            isPartOfIntelligentPool = isPartOfIntelligentPool,
            keepHiddenForSeconds = keepHiddenForSeconds,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            mode = mode,
            parallelSearchRequestCount = parallelSearchRequestCount,
            pickupZone = pickupZone,
            requestId = Kernel.Types.Id.Id requestId,
            response = response,
            rideFrequencyScore = rideFrequencyScore,
            rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
            searchRequestValidTill = Data.Time.localTimeToUTC Data.Time.utc createdAt,
            searchTryId = Kernel.Types.Id.Id searchTryId,
            startTime = startTime,
            status = status,
            straightLineDistanceToPickup = straightLineDistanceToPickup,
            vehicleServiceTier = Kernel.Prelude.fromMaybe (SharedLogic.DriverPool.Types.castVariantToServiceTier vehicleVariant) vehicleServiceTier,
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
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc searchRequestValidTill,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.customerCancellationDues = Kernel.Prelude.Just customerCancellationDues,
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
        Beam.durationToPickup = durationToPickup,
        Beam.estimateId = estimateId,
        Beam.goHomeRequestId = Kernel.Types.Id.getId <$> goHomeRequestId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isPartOfIntelligentPool = isPartOfIntelligentPool,
        Beam.keepHiddenForSeconds = keepHiddenForSeconds,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mode = mode,
        Beam.parallelSearchRequestCount = parallelSearchRequestCount,
        Beam.pickupZone = pickupZone,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.response = response,
        Beam.rideFrequencyScore = rideFrequencyScore,
        Beam.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
        Beam.searchRequestValidTill = Data.Time.utcToLocalTime Data.Time.utc searchRequestValidTill,
        Beam.searchTryId = Kernel.Types.Id.getId searchTryId,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.straightLineDistanceToPickup = straightLineDistanceToPickup,
        Beam.vehicleServiceTier = Kernel.Prelude.Just vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName,
        Beam.vehicleVariant = vehicleVariant
      }
