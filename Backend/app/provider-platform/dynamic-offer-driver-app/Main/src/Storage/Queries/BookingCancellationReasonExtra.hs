module Storage.Queries.BookingCancellationReasonExtra where

import qualified Data.List
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
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

-- Distinct bookingIds cancelled by this driver: per-ride ride rows unioned with BCR, nub-deduped.
findAllCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m Int
findAllCancelledByDriverId driverId = length <$> findAllBookingIdsCancelledByDriverId driverId

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BookingCancellationReason -> m ()
upsert cancellationReason = do
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.driverId (getId <$> cancellationReason.driverId),
          Se.Set BeamBCR.source cancellationReason.source,
          Se.Set BeamBCR.reasonCode ((\(CancellationReasonCode x) -> x) <$> cancellationReason.reasonCode),
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason
  -- Mirror onto the ride row (never overwritten) since BCR overwrites on a reused bookingId.
  whenJust cancellationReason.rideId $ \rideId ->
    QRide.updateCancellationDetails (Just $ show cancellationReason.source) cancellationReason.reasonCode cancellationReason.additionalInfo rideId

findAllBookingIdsCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = do
  bcrBks <- findAllWithDb [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> (DBCR.bookingId <$>)
  rideBks <- QRide.findCancelledBookingIdsByDriver driverId
  pure $ Data.List.nub (rideBks ++ bcrBks)
