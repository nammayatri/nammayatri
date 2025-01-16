{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicRollout where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout

instance FromTType' Beam.AppDynamicLogicRollout Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout where
  fromTType' (Beam.AppDynamicLogicRolloutT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout
          { domain = domain,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            percentageRollout = percentageRollout,
            timeBounds = timeBounds,
            version = version,
            versionDescription = versionDescription,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogicRollout Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout {..}) = do
    Beam.AppDynamicLogicRolloutT
      { Beam.domain = domain,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.percentageRollout = percentageRollout,
        Beam.timeBounds = timeBounds,
        Beam.version = version,
        Beam.versionDescription = versionDescription,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
