{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BookingCancellationReason where

import qualified Domain.Types.BookingCancellationReason
import qualified Domain.Types.CancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
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
            driverCancellationLocation = Kernel.External.Maps.LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
            driverDistToPickup = driverDistToPickup,
            driverId = Kernel.Types.Id.Id <$> driverId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            reasonCode = Domain.Types.CancellationReason.CancellationReasonCode <$> reasonCode,
            rideId = Kernel.Types.Id.Id <$> rideId,
            source = source,
            createdAt = createdAt',
            updatedAt = updatedAt'
          }

instance ToTType' Beam.BookingCancellationReason Domain.Types.BookingCancellationReason.BookingCancellationReason where
  toTType' (Domain.Types.BookingCancellationReason.BookingCancellationReason {..}) = do
    Beam.BookingCancellationReasonT
      { Beam.additionalInfo = additionalInfo,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.driverCancellationLocationLat = (driverCancellationLocation <&> (.lat)),
        Beam.driverCancellationLocationLon = (driverCancellationLocation <&> (.lon)),
        Beam.driverDistToPickup = driverDistToPickup,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.reasonCode = (\(Domain.Types.CancellationReason.CancellationReasonCode x) -> x) <$> reasonCode,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.source = source,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
