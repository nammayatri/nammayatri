{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SubscriptionTransaction where

import qualified Domain.Types.SubscriptionTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SubscriptionTransaction as Beam

instance FromTType' Beam.SubscriptionTransaction Domain.Types.SubscriptionTransaction.SubscriptionTransaction where
  fromTType' (Beam.SubscriptionTransactionT {..}) = do
    pure $
      Just
        Domain.Types.SubscriptionTransaction.SubscriptionTransaction
          { amount = amount,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            entityId = entityId,
            fleetOwnerId = Kernel.Types.Id.Id <$> fleetOwnerId,
            fromLocationId = Kernel.Types.Id.Id <$> fromLocationId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            runningBalance = runningBalance,
            status = status,
            toLocationId = Kernel.Types.Id.Id <$> toLocationId,
            transactionType = transactionType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SubscriptionTransaction Domain.Types.SubscriptionTransaction.SubscriptionTransaction where
  toTType' (Domain.Types.SubscriptionTransaction.SubscriptionTransaction {..}) = do
    Beam.SubscriptionTransactionT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.entityId = entityId,
        Beam.fleetOwnerId = Kernel.Types.Id.getId <$> fleetOwnerId,
        Beam.fromLocationId = Kernel.Types.Id.getId <$> fromLocationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.runningBalance = runningBalance,
        Beam.status = status,
        Beam.toLocationId = Kernel.Types.Id.getId <$> toLocationId,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt
      }
