{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Exophone where

import qualified Domain.Types.Exophone
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Exophone as Beam

instance FromTType' Beam.Exophone Domain.Types.Exophone.Exophone where
  fromTType' (Beam.ExophoneT {..}) = do
    pure $
      Just
        Domain.Types.Exophone.Exophone
          { backupPhone = backupPhone,
            callService = callService,
            createdAt = createdAt,
            enableAlternateNumber = enableAlternateNumber,
            id = Kernel.Types.Id.Id id,
            isPrimaryDown = isPrimaryDown,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            primaryPhone = primaryPhone,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Exophone Domain.Types.Exophone.Exophone where
  toTType' (Domain.Types.Exophone.Exophone {..}) = do
    Beam.ExophoneT
      { Beam.backupPhone = backupPhone,
        Beam.callService = callService,
        Beam.createdAt = createdAt,
        Beam.enableAlternateNumber = enableAlternateNumber,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isPrimaryDown = isPrimaryDown,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.primaryPhone = primaryPhone,
        Beam.updatedAt = updatedAt
      }
