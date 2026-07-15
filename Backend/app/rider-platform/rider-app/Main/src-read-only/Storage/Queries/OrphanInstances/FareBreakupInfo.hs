{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareBreakupInfo where

import qualified Domain.Types.FareBreakupInfo
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Storage.Beam.FareBreakupInfo as Beam

instance FromTType' Beam.FareBreakupInfo Domain.Types.FareBreakupInfo.FareBreakupInfo where
  fromTType' (Beam.FareBreakupInfoT {..}) = do
    pure $
      Just
        Domain.Types.FareBreakupInfo.FareBreakupInfo
          { entityId = entityId,
            entityType = entityType,
            fareBreakups = (fromMaybe [] (Kernel.Utils.Text.decodeFromText fareBreakups)),
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FareBreakupInfo Domain.Types.FareBreakupInfo.FareBreakupInfo where
  toTType' (Domain.Types.FareBreakupInfo.FareBreakupInfo {..}) = do
    Beam.FareBreakupInfoT
      { Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.fareBreakups = Kernel.Utils.Text.encodeToText fareBreakups,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
