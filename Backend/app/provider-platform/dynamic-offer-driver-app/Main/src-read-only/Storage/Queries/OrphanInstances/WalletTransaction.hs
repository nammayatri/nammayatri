{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.WalletTransaction where

import qualified Domain.Types.WalletTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.WalletTransaction as Beam

instance FromTType' Beam.WalletTransaction Domain.Types.WalletTransaction.WalletTransaction where
  fromTType' (Beam.WalletTransactionT {..}) = do
    pure $
      Just
        Domain.Types.WalletTransaction.WalletTransaction
          { amount = amount,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            status = status,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.WalletTransaction Domain.Types.WalletTransaction.WalletTransaction where
  toTType' (Domain.Types.WalletTransaction.WalletTransaction {..}) = do
    Beam.WalletTransactionT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
