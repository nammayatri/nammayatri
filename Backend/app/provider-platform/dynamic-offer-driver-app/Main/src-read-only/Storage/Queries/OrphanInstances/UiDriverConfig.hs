{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.UiDriverConfig where

import qualified Domain.Types.UiDriverConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.UiDriverConfig as Beam

instance FromTType' Beam.UiDriverConfig Domain.Types.UiDriverConfig.UiDriverConfig where
  fromTType' (Beam.UiDriverConfigT {..}) = do
    pure $
      Just
        Domain.Types.UiDriverConfig.UiDriverConfig
          { bundleVersion = bundleVersion,
            config = config,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            language = language,
            os = os,
            updatedAt = updatedAt,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.UiDriverConfig Domain.Types.UiDriverConfig.UiDriverConfig where
  toTType' (Domain.Types.UiDriverConfig.UiDriverConfig {..}) = do
    Beam.UiDriverConfigT
      { Beam.bundleVersion = bundleVersion,
        Beam.config = config,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.os = os,
        Beam.updatedAt = updatedAt,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
