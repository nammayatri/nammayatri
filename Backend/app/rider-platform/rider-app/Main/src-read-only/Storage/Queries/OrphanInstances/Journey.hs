{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Journey where

import qualified Domain.Types.Journey
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Journey as Beam

instance FromTType' Beam.Journey Domain.Types.Journey.Journey where
  fromTType' (Beam.JourneyT {..}) = do
    pure $
      Just
        Domain.Types.Journey.Journey
          { convenienceCost = convenienceCost,
            endTime = endTime,
            estimatedDistance = (Kernel.Types.Common.Distance estimatedDistance distanceUnit),
            estimatedDuration = estimatedDuration,
            id = Kernel.Types.Id.Id id,
            isPaymentSuccess = isPaymentSuccess,
            modes = modes,
            riderId = Kernel.Types.Id.Id riderId,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            startTime = startTime,
            status = (fromMaybe Domain.Types.Journey.NEW status),
            totalLegs = totalLegs,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Journey Domain.Types.Journey.Journey where
  toTType' (Domain.Types.Journey.Journey {..}) = do
    Beam.JourneyT
      { Beam.convenienceCost = convenienceCost,
        Beam.endTime = endTime,
        Beam.distanceUnit = ((.unit)) estimatedDistance,
        Beam.estimatedDistance = ((.value)) estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isPaymentSuccess = isPaymentSuccess,
        Beam.modes = modes,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.startTime = startTime,
        Beam.status = (Just status),
        Beam.totalLegs = totalLegs,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
