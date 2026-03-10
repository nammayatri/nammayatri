{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.StateEntryPermitCharges where

import qualified Domain.Types.StateEntryPermitCharges
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.StateEntryPermitCharges as Beam

instance FromTType' Beam.StateEntryPermitCharges Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges where
  fromTType' (Beam.StateEntryPermitChargesT {..}) = do
    pure $
      Just
        Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges
          { amount = amount,
            createdAt = createdAt,
            geomId = Kernel.Types.Id.Id geomId,
            id = Kernel.Types.Id.Id id,
            name = name,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.StateEntryPermitCharges Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges where
  toTType' (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges {..}) = do
    Beam.StateEntryPermitChargesT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.geomId = Kernel.Types.Id.getId geomId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
