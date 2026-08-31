{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourney where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as Beam

instance FromTType' Beam.IncentiveJourney Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney where
  fromTType' (Beam.IncentiveJourneyT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney
          { createdAt = createdAt,
            description = description,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            journeyType = journeyType,
            maxWaiveOffCount = maxWaiveOffCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IncentiveJourney Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney where
  toTType' (Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney {..}) = do
    Beam.IncentiveJourneyT
      { Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyType = journeyType,
        Beam.maxWaiveOffCount = maxWaiveOffCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.updatedAt = updatedAt
      }
