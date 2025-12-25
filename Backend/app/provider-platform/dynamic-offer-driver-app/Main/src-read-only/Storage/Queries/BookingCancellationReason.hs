{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingCancellationReason (module Storage.Queries.BookingCancellationReason, module ReExport) where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingCancellationReason
import qualified Domain.Types.CancellationReason
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as Beam
import Storage.Queries.BookingCancellationReasonExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingCancellationReason.BookingCancellationReason -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BookingCancellationReason.BookingCancellationReason] -> m ())
createMany = traverse_ create

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.BookingCancellationReason.BookingCancellationReason))
findByBookingId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findByRideId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride) -> m (Maybe Domain.Types.BookingCancellationReason.BookingCancellationReason))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId <$> rideId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.BookingCancellationReason.BookingCancellationReason))
findByPrimaryKey bookingId = do findOneWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingCancellationReason.BookingCancellationReason -> m ())
updateByPrimaryKey (Domain.Types.BookingCancellationReason.BookingCancellationReason {..}) = do
  updateWithKV
    [ Se.Set Beam.additionalInfo additionalInfo,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverCancellationLocationLat (driverCancellationLocation <&> (.lat)),
      Se.Set Beam.driverCancellationLocationLon (driverCancellationLocation <&> (.lon)),
      Se.Set Beam.driverDistToPickup driverDistToPickup,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.reasonCode ((\(Domain.Types.CancellationReason.CancellationReasonCode x) -> x) <$> reasonCode),
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.source source
    ]
    [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]
