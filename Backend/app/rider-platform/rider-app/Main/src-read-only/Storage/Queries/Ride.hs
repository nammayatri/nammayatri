{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Ride (module Storage.Queries.Ride, module ReExport) where

import qualified Domain.Types.Booking
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Ride as Beam
import Storage.Queries.RideExtra as ReExport

findByBPPRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.BPPRide -> m (Maybe Domain.Types.Ride.Ride))
findByBPPRideId bppRideId = do findOneWithKV [Se.Is Beam.bppRideId $ Se.Eq (Kernel.Types.Id.getId bppRideId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.Ride.Ride))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRBId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.Ride.Ride))
findByRBId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findRideByRideShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Domain.Types.Ride.Ride -> m (Maybe Domain.Types.Ride.Ride))
findRideByRideShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

markPaymentDone :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
markPaymentDone paymentDone id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.paymentDone (Kernel.Prelude.Just paymentDone), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationFeeIfCancelledField :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateCancellationFeeIfCancelledField cancellationFeeIfCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationFeeIfCancelled cancellationFeeIfCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
