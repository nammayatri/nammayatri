{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverBlockReason where

import qualified Domain.Types.DriverBlockReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverBlockReason as Beam

instance FromTType' Beam.DriverBlockReason Domain.Types.DriverBlockReason.DriverBlockReason where
  fromTType' (Beam.DriverBlockReasonT {..}) = do
    pure $
      Just
        Domain.Types.DriverBlockReason.DriverBlockReason
          { blockReason = blockReason,
            blockTimeInHours = blockTimeInHours,
            reasonCode = Kernel.Types.Id.Id reasonCode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverBlockReason Domain.Types.DriverBlockReason.DriverBlockReason where
  toTType' (Domain.Types.DriverBlockReason.DriverBlockReason {..}) = do
    Beam.DriverBlockReasonT
      { Beam.blockReason = blockReason,
        Beam.blockTimeInHours = blockTimeInHours,
        Beam.reasonCode = Kernel.Types.Id.getId reasonCode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
