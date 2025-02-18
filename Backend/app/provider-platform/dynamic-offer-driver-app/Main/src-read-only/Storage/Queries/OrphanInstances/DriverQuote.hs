{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverQuote where

import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.Common
import qualified Domain.Types.DriverQuote
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
import qualified Storage.Beam.DriverQuote as Beam
import Storage.Queries.Transformers.DriverQuote

instance FromTType' Beam.DriverQuote Domain.Types.DriverQuote.DriverQuote where
  fromTType' (Beam.DriverQuoteT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    fareParams' <- getFareParams fareParametersId
    pure $
      Just
        Domain.Types.DriverQuote.DriverQuote
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientId = clientId,
            clientSdkVersion = clientSdkVersion',
            createdAt = Data.Time.localTimeToUTC Data.Time.utc createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            distance = distance,
            distanceToPickup = distanceToPickup,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            driverRating = driverRating,
            durationToPickup = durationToPickup,
            estimateId = Kernel.Types.Id.Id estimateId,
            estimatedFare = Kernel.Types.Common.mkAmountWithDefault estimatedFareAmount estimatedFare,
            fareParams = fareParams',
            goHomeRequestId = Kernel.Types.Id.Id <$> goHomeRequestId,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            providerId = Kernel.Types.Id.Id providerId,
            requestId = Kernel.Types.Id.Id requestId,
            searchRequestForDriverId = Kernel.Types.Id.Id <$> searchRequestForDriverId,
            searchTryId = Kernel.Types.Id.Id searchTryId,
            specialLocationTag = specialLocationTag,
            status = status,
            tripCategory = fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer) tripCategory,
            updatedAt = Data.Time.localTimeToUTC Data.Time.utc updatedAt,
            validTill = Data.Time.localTimeToUTC Data.Time.utc validTill,
            vehicleServiceTier = fromMaybe (Domain.Types.VehicleVariant.castVariantToServiceTier vehicleVariant) vehicleServiceTier,
            vehicleServiceTierName = vehicleServiceTierName,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverQuote Domain.Types.DriverQuote.DriverQuote where
  toTType' (Domain.Types.DriverQuote.DriverQuote {..}) = do
    Beam.DriverQuoteT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = clientId,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distance = distance,
        Beam.distanceToPickup = distanceToPickup,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.driverRating = driverRating,
        Beam.durationToPickup = durationToPickup,
        Beam.estimateId = Kernel.Types.Id.getId estimateId,
        Beam.estimatedFare = Kernel.Prelude.roundToIntegral estimatedFare,
        Beam.estimatedFareAmount = Kernel.Prelude.Just estimatedFare,
        Beam.fareParametersId = Kernel.Types.Id.getId ((.id) fareParams),
        Beam.goHomeRequestId = Kernel.Types.Id.getId <$> goHomeRequestId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.providerId = Kernel.Types.Id.getId providerId,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.searchRequestForDriverId = Kernel.Types.Id.getId <$> searchRequestForDriverId,
        Beam.searchTryId = Kernel.Types.Id.getId searchTryId,
        Beam.specialLocationTag = specialLocationTag,
        Beam.status = status,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = Data.Time.utcToLocalTime Data.Time.utc updatedAt,
        Beam.validTill = Data.Time.utcToLocalTime Data.Time.utc validTill,
        Beam.vehicleServiceTier = Kernel.Prelude.Just vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName,
        Beam.vehicleVariant = vehicleVariant
      }
