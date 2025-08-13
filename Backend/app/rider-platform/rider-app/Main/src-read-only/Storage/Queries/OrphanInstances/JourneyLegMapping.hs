{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.JourneyLegMapping where

import qualified Domain.Types.JourneyLegMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.JourneyLegMapping as Beam

instance FromTType' Beam.JourneyLegMapping Domain.Types.JourneyLegMapping.JourneyLegMapping where
  fromTType' (Beam.JourneyLegMappingT {..}) = do
    pure $
      Just
        Domain.Types.JourneyLegMapping.JourneyLegMapping
          { id = Kernel.Types.Id.Id id,
            isDeleted = isDeleted,
            journeyId = Kernel.Types.Id.Id journeyId,
            journeyLegId = Kernel.Types.Id.Id journeyLegId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            sequenceNumber = sequenceNumber,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLegMapping Domain.Types.JourneyLegMapping.JourneyLegMapping where
  toTType' (Domain.Types.JourneyLegMapping.JourneyLegMapping {..}) = do
    Beam.JourneyLegMappingT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.journeyLegId = Kernel.Types.Id.getId journeyLegId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.sequenceNumber = sequenceNumber,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
