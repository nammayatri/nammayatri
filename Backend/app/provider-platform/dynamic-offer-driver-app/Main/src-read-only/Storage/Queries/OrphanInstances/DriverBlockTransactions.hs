{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverBlockTransactions where

import qualified Domain.Types.DriverBlockTransactions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverBlockTransactions as Beam

instance FromTType' Beam.DriverBlockTransactions Domain.Types.DriverBlockTransactions.DriverBlockTransactions where
  fromTType' (Beam.DriverBlockTransactionsT {..}) = do
    pure $
      Just
        Domain.Types.DriverBlockTransactions.DriverBlockTransactions
          { blockLiftTime = blockLiftTime,
            blockReason = blockReason,
            blockTimeInHours = blockTimeInHours,
            blockedBy = blockedBy,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            reasonCode = reasonCode,
            reportedAt = reportedAt,
            requestorId = requestorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverBlockTransactions Domain.Types.DriverBlockTransactions.DriverBlockTransactions where
  toTType' (Domain.Types.DriverBlockTransactions.DriverBlockTransactions {..}) = do
    Beam.DriverBlockTransactionsT
      { Beam.blockLiftTime = blockLiftTime,
        Beam.blockReason = blockReason,
        Beam.blockTimeInHours = blockTimeInHours,
        Beam.blockedBy = blockedBy,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reasonCode = reasonCode,
        Beam.reportedAt = reportedAt,
        Beam.requestorId = requestorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
