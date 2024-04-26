{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BusinessEvent where

import qualified Domain.Types.BusinessEvent
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BusinessEvent as Beam
import Storage.Queries.Transformers.BusinessEvent

instance FromTType' Beam.BusinessEvent Domain.Types.BusinessEvent.BusinessEvent where
  fromTType' (Beam.BusinessEventT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $
      Just
        Domain.Types.BusinessEvent.BusinessEvent
          { bookingId = Kernel.Types.Id.Id <$> bookingId,
            createdAt = createdAt',
            distance = Kernel.Types.Common.Meters <$> distance,
            driverId = Kernel.Types.Id.Id <$> driverId,
            duration = Kernel.Types.Common.Seconds <$> distance,
            eventType = eventType,
            id = Kernel.Types.Id.Id id,
            rideId = Kernel.Types.Id.Id <$> rideId,
            timeStamp = timeStamp,
            updatedAt = updatedAt',
            vehicleVariant = vehicleVariant,
            whenPoolWasComputed = whenPoolWasComputed
          }

instance ToTType' Beam.BusinessEvent Domain.Types.BusinessEvent.BusinessEvent where
  toTType' (Domain.Types.BusinessEvent.BusinessEvent {..}) = do
    Beam.BusinessEventT
      { Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.distance = Kernel.Types.Common.getMeters <$> distance,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.duration = Kernel.Types.Common.getSeconds <$> duration,
        Beam.eventType = eventType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.timeStamp = timeStamp,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.vehicleVariant = vehicleVariant,
        Beam.whenPoolWasComputed = whenPoolWasComputed
      }
