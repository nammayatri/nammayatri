{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.AutoApplyCohortMapping where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping
import qualified Lib.IncentiveJourney.Storage.Beam.AutoApplyCohortMapping as Beam

instance FromTType' Beam.AutoApplyCohortMapping Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping where
  fromTType' (Beam.AutoApplyCohortMappingT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping
          { allowIfNoMapping = allowIfNoMapping,
            cohortJourneyMappingId = Kernel.Types.Id.Id cohortJourneyMappingId,
            createdAt = createdAt,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt,
            vehicleCategory = ((Kernel.Prelude.maybe Nothing (Kernel.Prelude.readMaybe . Data.Text.unpack))) vehicleCategory
          }

instance ToTType' Beam.AutoApplyCohortMapping Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping where
  toTType' (Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping {..}) = do
    Beam.AutoApplyCohortMappingT
      { Beam.allowIfNoMapping = allowIfNoMapping,
        Beam.cohortJourneyMappingId = Kernel.Types.Id.getId cohortJourneyMappingId,
        Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = ((Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show))) vehicleCategory
      }
