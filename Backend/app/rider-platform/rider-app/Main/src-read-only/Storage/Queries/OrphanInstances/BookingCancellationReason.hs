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
          { bookingId = Kernel.Types.Id.Id bookingId,
            rideId = Kernel.Types.Id.Id <$> rideId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            source = source,
            reasonCode = reasonCode,
            reasonStage = reasonStage,
            additionalInfo = additionalInfo,
            driverCancellationLocation = Kernel.External.Maps.LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
            driverDistToPickup = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit driverDistToPickupValue <$> driverDistToPickup,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            createdAt = createdAt',
            updatedAt = updatedAt'
          }

instance ToTType' Beam.BookingCancellationReason Domain.Types.BookingCancellationReason.BookingCancellationReason where
  toTType' (Domain.Types.BookingCancellationReason.BookingCancellationReason {..}) = do
    Beam.BookingCancellationReasonT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.source = source,
        Beam.reasonCode = reasonCode,
        Beam.reasonStage = reasonStage,
        Beam.additionalInfo = additionalInfo,
        Beam.driverCancellationLocationLat = driverCancellationLocation <&> (.lat),
        Beam.driverCancellationLocationLon = driverCancellationLocation <&> (.lon),
        Beam.driverDistToPickup = Kernel.Types.Common.distanceToMeters <$> driverDistToPickup,
        Beam.driverDistToPickupValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> driverDistToPickup,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
