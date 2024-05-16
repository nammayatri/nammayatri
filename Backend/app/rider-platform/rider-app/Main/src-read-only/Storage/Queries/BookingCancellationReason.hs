{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingCancellationReason (module Storage.Queries.BookingCancellationReason, module ReExport) where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingCancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as Beam
import Storage.Queries.BookingCancellationReasonExtra as ReExport
import Storage.Queries.Transformers.BookingCancellationReason

create :: KvDbFlow m r => (Domain.Types.BookingCancellationReason.BookingCancellationReason -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BookingCancellationReason.BookingCancellationReason] -> m ())
createMany = traverse_ create

findByRideBookingId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.BookingCancellationReason.BookingCancellationReason))
findByRideBookingId (Kernel.Types.Id.Id bookingId) = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq bookingId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.BookingCancellationReason.BookingCancellationReason))
findByPrimaryKey (Kernel.Types.Id.Id bookingId) = do findOneWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq bookingId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BookingCancellationReason.BookingCancellationReason -> m ())
updateByPrimaryKey (Domain.Types.BookingCancellationReason.BookingCancellationReason {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.additionalInfo additionalInfo,
      Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt),
      Se.Set Beam.driverCancellationLocationLat (driverCancellationLocation <&> (.lat)),
      Se.Set Beam.driverCancellationLocationLon (driverCancellationLocation <&> (.lon)),
      Se.Set Beam.distanceUnit (driverDistToPickup <&> (.unit)),
      Se.Set Beam.driverDistToPickup (Kernel.Types.Common.distanceToMeters <$> driverDistToPickup),
      Se.Set Beam.driverDistToPickupValue (Kernel.Types.Common.distanceToHighPrecDistance (driverDistToPickup <&> (.unit)) <$> driverDistToPickup),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.reasonCode reasonCode,
      Se.Set Beam.reasonStage reasonStage,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.source source,
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]
