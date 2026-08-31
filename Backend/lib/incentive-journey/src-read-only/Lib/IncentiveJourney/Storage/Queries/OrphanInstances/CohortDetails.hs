{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.CohortDetails where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails
import qualified Lib.IncentiveJourney.Storage.Beam.CohortDetails as Beam

instance FromTType' Beam.CohortDetails Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails where
  fromTType' (Beam.CohortDetailsT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            name = name,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CohortDetails Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails where
  toTType' (Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails {..}) = do
    Beam.CohortDetailsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.updatedAt = updatedAt
      }
