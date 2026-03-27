{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicAlwaysOn where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicAlwaysOn as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn

instance FromTType' Beam.AppDynamicLogicAlwaysOn Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn where
  fromTType' (Beam.AppDynamicLogicAlwaysOnT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn
          { domain = domain,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            order = order,
            version = version,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogicAlwaysOn Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn {..}) = do
    Beam.AppDynamicLogicAlwaysOnT
      { Beam.domain = domain,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.order = order,
        Beam.version = version,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
