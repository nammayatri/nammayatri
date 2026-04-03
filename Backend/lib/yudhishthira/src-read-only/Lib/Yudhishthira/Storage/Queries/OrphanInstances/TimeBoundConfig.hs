{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Yudhishthira.Storage.Queries.OrphanInstances.TimeBoundConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Yudhishthira.Types.TimeBoundConfig
import qualified Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as Beam
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types



instance FromTType' Beam.TimeBoundConfig Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig
    where fromTType' (Beam.TimeBoundConfigT {..}) = do pure $ Just Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig{merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                          name = name,
                                                                                                                          timeBoundDomain = timeBoundDomain,
                                                                                                                          timeBounds = timeBounds,
                                                                                                                          createdAt = createdAt,
                                                                                                                          updatedAt = updatedAt}
instance ToTType' Beam.TimeBoundConfig Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig
    where toTType' (Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig {..}) = do Beam.TimeBoundConfigT{Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                            Beam.name = name,
                                                                                                            Beam.timeBoundDomain = timeBoundDomain,
                                                                                                            Beam.timeBounds = timeBounds,
                                                                                                            Beam.createdAt = createdAt,
                                                                                                            Beam.updatedAt = updatedAt}



