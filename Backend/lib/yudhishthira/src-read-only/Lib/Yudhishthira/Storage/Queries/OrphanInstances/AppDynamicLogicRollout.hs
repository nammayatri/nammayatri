{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicRollout where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Types.Id



instance FromTType' Beam.AppDynamicLogicRollout Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout
    where fromTType' (Beam.AppDynamicLogicRolloutT {..}) = do pure $ Just Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout{domain = domain,
                                                                                                                                               experimentStatus = experimentStatus,
                                                                                                                                               isBaseVersion = isBaseVersion,
                                                                                                                                               merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                                               merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                               modifiedBy = Kernel.Types.Id.Id <$> modifiedBy,
                                                                                                                                               percentageRollout = percentageRollout,
                                                                                                                                               timeBounds = timeBounds,
                                                                                                                                               version = version,
                                                                                                                                               versionDescription = versionDescription,
                                                                                                                                               createdAt = createdAt,
                                                                                                                                               updatedAt = updatedAt}
instance ToTType' Beam.AppDynamicLogicRollout Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout
    where toTType' (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout {..}) = do Beam.AppDynamicLogicRolloutT{Beam.domain = domain,
                                                                                                                                 Beam.experimentStatus = experimentStatus,
                                                                                                                                 Beam.isBaseVersion = isBaseVersion,
                                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                                                 Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                                 Beam.modifiedBy = Kernel.Types.Id.getId <$> modifiedBy,
                                                                                                                                 Beam.percentageRollout = percentageRollout,
                                                                                                                                 Beam.timeBounds = timeBounds,
                                                                                                                                 Beam.version = version,
                                                                                                                                 Beam.versionDescription = versionDescription,
                                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                                 Beam.updatedAt = updatedAt}



