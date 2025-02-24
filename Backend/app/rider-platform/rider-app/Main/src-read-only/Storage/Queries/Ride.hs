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

markPaymentStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Ride.PaymentStatus -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
markPaymentStatus paymentStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.paymentStatus (Kernel.Prelude.Just paymentStatus), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationFeeIfCancelledField :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateCancellationFeeIfCancelledField cancellationFeeIfCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationFeeIfCancelled cancellationFeeIfCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDestinationReachedAt :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateDestinationReachedAt destinationReachedAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.destinationReachedAt destinationReachedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateEstimatedEndTimeRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.Ride.EstimatedEndTimeRange -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateEstimatedEndTimeRange estimatedEndTimeRange id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.estimatedEndTimeRangeEnd (Kernel.Prelude.fmap (.end) estimatedEndTimeRange),
      Se.Set Beam.estimatedEndTimeRangeStart (Kernel.Prelude.fmap (.start) estimatedEndTimeRange),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFeedbackSkipped :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateFeedbackSkipped feedbackSkipped id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.feedbackSkipped (Kernel.Prelude.Just feedbackSkipped), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePickupRouteCallCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updatePickupRouteCallCount pickupRouteCallCount id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.pickupRouteCallCount pickupRouteCallCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTalkedWithDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateTalkedWithDriver talkedWithDriver id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.talkedWithDriver talkedWithDriver, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTipByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateTipByRideId tipAmount id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.tipAmount (Kernel.Prelude.fmap (.amount) tipAmount), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
