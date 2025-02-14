{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.UiRiderConfig where

import qualified Domain.Types.UiRiderConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.UiRiderConfig as Beam

instance FromTType' Beam.UiRiderConfig Domain.Types.UiRiderConfig.UiRiderConfig where
  fromTType' (Beam.UiRiderConfigT {..}) = do
    pure $
      Just
        Domain.Types.UiRiderConfig.UiRiderConfig
          { config = config,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            os = os,
            platform = platform,
            updatedAt = updatedAt
          }

instance ToTType' Beam.UiRiderConfig Domain.Types.UiRiderConfig.UiRiderConfig where
  toTType' (Domain.Types.UiRiderConfig.UiRiderConfig {..}) = do
    Beam.UiRiderConfigT
      { Beam.config = config,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.os = os,
        Beam.platform = platform,
        Beam.updatedAt = updatedAt
      }
