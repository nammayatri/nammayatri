{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideDetails where

import qualified Domain.Types.Ride
import qualified Domain.Types.RideDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RideDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideDetails.RideDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideDetails.RideDetails] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.RideDetails.RideDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.RideDetails.RideDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideDetails.RideDetails -> m ())
updateByPrimaryKey (Domain.Types.RideDetails.RideDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.defaultServiceTierName defaultServiceTierName,
      Se.Set Beam.driverCountryCode driverCountryCode,
      Se.Set Beam.driverName driverName,
      Se.Set Beam.driverNumberEncrypted (driverNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.driverNumberHash (driverNumber <&> (.hash)),
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.vehicleAge vehicleAge,
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

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
