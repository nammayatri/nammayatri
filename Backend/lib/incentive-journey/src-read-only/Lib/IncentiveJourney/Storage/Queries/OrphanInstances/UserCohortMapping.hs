{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.UserCohortMapping where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping
import qualified Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as Beam

instance FromTType' Beam.UserCohortMapping Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping where
  fromTType' (Beam.UserCohortMappingT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping
          { cohortMappingId = Kernel.Types.Id.Id cohortMappingId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            isTestGroup = isTestGroup,
            updatedAt = updatedAt,
            userId = Kernel.Types.Id.Id userId
          }

instance ToTType' Beam.UserCohortMapping Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping where
  toTType' (Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping {..}) = do
    Beam.UserCohortMappingT
      { Beam.cohortMappingId = Kernel.Types.Id.getId cohortMappingId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isTestGroup = isTestGroup,
        Beam.updatedAt = updatedAt,
        Beam.userId = Kernel.Types.Id.getId userId
      }
