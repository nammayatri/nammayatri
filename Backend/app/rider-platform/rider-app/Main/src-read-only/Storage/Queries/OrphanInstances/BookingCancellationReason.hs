{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BookingCancellationReason where

import qualified Domain.Types.BookingCancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BookingCancellationReason as Beam
import Storage.Queries.Transformers.BookingCancellationReason

instance FromTType' Beam.BookingCancellationReason Domain.Types.BookingCancellationReason.BookingCancellationReason where
  fromTType' (Beam.BookingCancellationReasonT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $
      Just
        Domain.Types.BookingCancellationReason.BookingCancellationReason
          { additionalInfo = additionalInfo,
            bookingId = Kernel.Types.Id.Id bookingId,
            createdAt = createdAt',
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverCancellationLocation = Kernel.External.Maps.LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
            driverDistToPickup = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit driverDistToPickupValue <$> driverDistToPickup,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            reasonCode = reasonCode,
            reasonStage = reasonStage,
            rideId = Kernel.Types.Id.Id <$> rideId,
            riderId = Kernel.Types.Id.Id <$> riderId,
            source = source,
            updatedAt = updatedAt'
          }

instance ToTType' Beam.BookingCancellationReason Domain.Types.BookingCancellationReason.BookingCancellationReason where
  toTType' (Domain.Types.BookingCancellationReason.BookingCancellationReason {..}) = do
    Beam.BookingCancellationReasonT
      { Beam.additionalInfo = additionalInfo,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverCancellationLocationLat = driverCancellationLocation <&> (.lat),
        Beam.driverCancellationLocationLon = driverCancellationLocation <&> (.lon),
        Beam.driverDistToPickup = Kernel.Types.Common.distanceToMeters <$> driverDistToPickup,
        Beam.driverDistToPickupValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> driverDistToPickup,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.reasonCode = reasonCode,
        Beam.reasonStage = reasonStage,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.riderId = Kernel.Types.Id.getId <$> riderId,
        Beam.source = source,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
