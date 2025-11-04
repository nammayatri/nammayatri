{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RideDetails where

import qualified Domain.Types.RideDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RideDetails as Beam

instance FromTType' Beam.RideDetails Domain.Types.RideDetails.RideDetails where
  fromTType' (Beam.RideDetailsT {..}) = do
    pure $
      Just
        Domain.Types.RideDetails.RideDetails
          { createdAt = createdAt,
            defaultServiceTierName = defaultServiceTierName,
            driverCountryCode = driverCountryCode,
            driverName = driverName,
            driverNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            vehicleAge = vehicleAge,
            vehicleClass = vehicleClass,
            vehicleColor = vehicleColor,
            vehicleModel = vehicleModel,
            vehicleNumber = vehicleNumber,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.RideDetails Domain.Types.RideDetails.RideDetails where
  toTType' (Domain.Types.RideDetails.RideDetails {..}) = do
    Beam.RideDetailsT
      { Beam.createdAt = createdAt,
        Beam.defaultServiceTierName = defaultServiceTierName,
        Beam.driverCountryCode = driverCountryCode,
        Beam.driverName = driverName,
        Beam.driverNumberEncrypted = driverNumber <&> unEncrypted . (.encrypted),
        Beam.driverNumberHash = driverNumber <&> (.hash),
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.vehicleAge = vehicleAge,
        Beam.vehicleClass = vehicleClass,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleVariant = vehicleVariant
      }
