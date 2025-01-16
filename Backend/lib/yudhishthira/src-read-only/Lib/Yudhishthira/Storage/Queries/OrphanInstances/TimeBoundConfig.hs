{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.TimeBoundConfig where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.TimeBoundConfig

instance FromTType' Beam.TimeBoundConfig Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig where
  fromTType' (Beam.TimeBoundConfigT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig
          { merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            timeBoundDomain = timeBoundDomain,
            timeBounds = timeBounds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TimeBoundConfig Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig where
  toTType' (Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig {..}) = do
    Beam.TimeBoundConfigT
      { Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.timeBoundDomain = timeBoundDomain,
        Beam.timeBounds = timeBounds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
