{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicExperimentGroup where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicExperimentGroup as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup

instance FromTType' Beam.AppDynamicLogicExperimentGroup Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup where
  fromTType' (Beam.AppDynamicLogicExperimentGroupT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup
          { experimentStatus = experimentStatus,
            groupName = groupName,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            modifiedBy = Kernel.Types.Id.Id <$> modifiedBy,
            rolloutPercentage = rolloutPercentage,
            timeBounds = timeBounds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogicExperimentGroup Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup {..}) = do
    Beam.AppDynamicLogicExperimentGroupT
      { Beam.experimentStatus = experimentStatus,
        Beam.groupName = groupName,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.modifiedBy = Kernel.Types.Id.getId <$> modifiedBy,
        Beam.rolloutPercentage = rolloutPercentage,
        Beam.timeBounds = timeBounds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
