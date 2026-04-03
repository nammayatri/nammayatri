{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.UiDriverConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.UiDriverConfig
import qualified Storage.Beam.UiDriverConfig as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.UiDriverConfig Domain.Types.UiDriverConfig.UiDriverConfig
    where fromTType' (Beam.UiDriverConfigT {..}) = do pure $ Just Domain.Types.UiDriverConfig.UiDriverConfig{config = config,
                                                                                                             createdAt = createdAt,
                                                                                                             id = Kernel.Types.Id.Id id,
                                                                                                             merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                             os = os,
                                                                                                             platform = platform,
                                                                                                             updatedAt = updatedAt}
instance ToTType' Beam.UiDriverConfig Domain.Types.UiDriverConfig.UiDriverConfig
    where toTType' (Domain.Types.UiDriverConfig.UiDriverConfig {..}) = do Beam.UiDriverConfigT{Beam.config = config,
                                                                                               Beam.createdAt = createdAt,
                                                                                               Beam.id = Kernel.Types.Id.getId id,
                                                                                               Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                               Beam.os = os,
                                                                                               Beam.platform = platform,
                                                                                               Beam.updatedAt = updatedAt}



