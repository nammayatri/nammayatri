{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TimeBoundConfig where

import qualified Domain.Types.TimeBoundConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TimeBoundConfig as Beam

instance FromTType' Beam.TimeBoundConfig Domain.Types.TimeBoundConfig.TimeBoundConfig where
  fromTType' (Beam.TimeBoundConfigT {..}) = do
    pure $
      Just
        Domain.Types.TimeBoundConfig.TimeBoundConfig
          { merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            timeBoundDomain = timeBoundDomain,
            timeBounds = timeBounds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TimeBoundConfig Domain.Types.TimeBoundConfig.TimeBoundConfig where
  toTType' (Domain.Types.TimeBoundConfig.TimeBoundConfig {..}) = do
    Beam.TimeBoundConfigT
      { Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.timeBoundDomain = timeBoundDomain,
        Beam.timeBounds = timeBounds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
