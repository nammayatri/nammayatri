{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SharedEntity where

import qualified Data.Aeson
import qualified Domain.Types.SharedEntity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SharedEntity as Beam

instance FromTType' Beam.SharedEntity Domain.Types.SharedEntity.SharedEntity where
  fromTType' (Beam.SharedEntityT {..}) = do
    pure $
      Just
        Domain.Types.SharedEntity.SharedEntity
          { bookingIds = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< bookingIds,
            counterAppSharedEntityId = counterAppSharedEntityId,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id <$> driverId,
            entityType = entityType,
            estimateIds = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< estimateIds,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pairingTime = pairingTime,
            pooledUsingCustomer = Kernel.Types.Id.Id <$> pooledUsingCustomer,
            rideIds = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< rideIds,
            searchRequestIds = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< searchRequestIds,
            status = status,
            totalSeats = totalSeats,
            transactionId = transactionId,
            tripCategory = tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory,
            waypoints = waypoints
          }

instance ToTType' Beam.SharedEntity Domain.Types.SharedEntity.SharedEntity where
  toTType' (Domain.Types.SharedEntity.SharedEntity {..}) = do
    Beam.SharedEntityT
      { Beam.bookingIds = Data.Aeson.toJSON <$> bookingIds,
        Beam.counterAppSharedEntityId = counterAppSharedEntityId,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.entityType = entityType,
        Beam.estimateIds = Data.Aeson.toJSON <$> estimateIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pairingTime = pairingTime,
        Beam.pooledUsingCustomer = Kernel.Types.Id.getId <$> pooledUsingCustomer,
        Beam.rideIds = Data.Aeson.toJSON <$> rideIds,
        Beam.searchRequestIds = Data.Aeson.toJSON <$> searchRequestIds,
        Beam.status = status,
        Beam.totalSeats = totalSeats,
        Beam.transactionId = transactionId,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory,
        Beam.waypoints = waypoints
      }
