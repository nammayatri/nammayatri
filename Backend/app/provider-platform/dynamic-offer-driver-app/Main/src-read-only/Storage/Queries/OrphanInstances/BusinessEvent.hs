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

instance FromTType' Beam.BusinessEvent Domain.Types.BusinessEvent.BusinessEvent where
  fromTType' (Beam.BusinessEventT {..}) = do
    pure $
      Just
        Domain.Types.BusinessEvent.BusinessEvent
          { bookingId = Kernel.Types.Id.Id <$> bookingId,
            distance = Kernel.Types.Common.Meters <$> distance,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id <$> driverId,
            duration = Kernel.Types.Common.Seconds <$> distance,
            eventType = eventType,
            id = Kernel.Types.Id.Id id,
            rideId = Kernel.Types.Id.Id <$> rideId,
            timeStamp = timeStamp,
            vehicleVariant = vehicleVariant,
            whenPoolWasComputed = whenPoolWasComputed
          }

instance ToTType' Beam.BusinessEvent Domain.Types.BusinessEvent.BusinessEvent where
  toTType' (Domain.Types.BusinessEvent.BusinessEvent {..}) = do
    Beam.BusinessEventT
      { Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.distance = Kernel.Types.Common.getMeters <$> distance,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.duration = Kernel.Types.Common.getSeconds <$> duration,
        Beam.eventType = eventType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.timeStamp = timeStamp,
        Beam.vehicleVariant = vehicleVariant,
        Beam.whenPoolWasComputed = whenPoolWasComputed
      }
