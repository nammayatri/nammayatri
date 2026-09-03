module Storage.Queries.BookingCancellationReasonExtra where

import qualified Data.Set as Set
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
import Domain.Types.Ride (Ride)
import EulerHS.Prelude as P hiding (null, (^.))
import Kernel.Beam.Functions
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import Storage.Queries.OrphanInstances.BookingCancellationReason ()
import qualified Storage.Queries.Ride as QRide

-- Extra code goes here --

-- Distinct bookingIds cancelled by this driver: per-ride ride rows unioned with BCR, Set-deduped.
-- The count needs the same distinct union, so it cannot be a DB COUNT(*): the two halves overlap
-- (one upsert writes both), and findAllWithDb is single-table, so no COUNT(DISTINCT) across them.
findAllCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m Int
findAllCancelledByDriverId driverId = Set.size <$> cancelledBookingIdSetByDriverId driverId

-- Batched BCR lookup by rideId for ride-level cancellation attribution (dashboard ride list fallback).
findAllByRideIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Ride] -> m [BookingCancellationReason]
findAllByRideIds rideIds = findAllWithKV [Se.Is BeamBCR.rideId $ Se.In $ (Just . getId) <$> rideIds]

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BookingCancellationReason -> m ()
upsert cancellationReason = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.reasonCode ((\(CancellationReasonCode x) -> x) <$> cancellationReason.reasonCode),
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo,
          Se.Set BeamBCR.ondcCancellationReasonId cancellationReason.ondcCancellationReasonId,
          Se.Set BeamBCR.updatedAt (Just now)
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason
  -- BCR is keyed by bookingId and a reused booking (BPP reallocation) rewrites that row, so the
  -- per-ride columns carry the real attribution. updateCancellationDetails is an unconditional
  -- Se.Set, so guard it: first writer wins, a later upsert must not relabel an attributed ride.
  whenJust cancellationReason.rideId $ \rideId -> do
    mbRide <- QRide.findById rideId
    when (maybe True (isNothing . (.cancelledBy)) mbRide) $
      QRide.updateCancellationDetails (Just $ show cancellationReason.source) cancellationReason.reasonCode cancellationReason.additionalInfo rideId

cancelledBookingIdSetByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Set.Set (Id Booking))
cancelledBookingIdSetByDriverId driverId = do
  bcrBks <- findAllWithDb [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> (DBCR.bookingId <$>)
  rideBks <- QRide.findCancelledBookingIdsByDriver driverId
  pure $ Set.fromList (rideBks <> bcrBks)

findAllBookingIdsCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = Set.toList <$> cancelledBookingIdSetByDriverId driverId
