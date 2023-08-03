{-# OPTIONS_GHC -Wno-orphans #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
import Domain.Types.Ride
import qualified EulerHS.Language as L
import EulerHS.Prelude as P hiding ((^.))
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
  ( FromTType' (fromTType'),
    ToTType' (toTType'),
    createWithKV,
    findAllWithKV,
    findAllWithKvInReplica,
    findOneWithKV,
    findOneWithKvInReplica,
    updateOneWithKV,
  )
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR

create :: (L.MonadFlow m, Log m) => DBCR.BookingCancellationReason -> m ()
create = createWithKV

-- TODO: Convert this function
-- findAllCancelledByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m Int
-- findAllCancelledByDriverId driverId = do
--   mkCount <$> do
--     Esq.findAll $ do
--       rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
--         rideBookingCancellationReason ^. BookingCancellationReasonDriverId ==. val (Just $ toKey driverId)
--           &&. rideBookingCancellationReason ^. BookingCancellationReasonSource ==. val ByDriver
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter

findAllCancelledByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m Int
findAllCancelledByDriverId driverId = findAllWithKV [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> length

findAllCancelledByDriverIdInReplica :: (L.MonadFlow m, Log m) => Id Person -> m Int
findAllCancelledByDriverIdInReplica driverId = findAllWithKvInReplica [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> length

findByBookingId :: (L.MonadFlow m, Log m) => Id Booking -> m (Maybe BookingCancellationReason)
findByBookingId (Id bookingId) = findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq bookingId]

findByBookingIdInReplica :: (L.MonadFlow m, Log m) => Id Booking -> m (Maybe BookingCancellationReason)
findByBookingIdInReplica (Id bookingId) = findOneWithKvInReplica [Se.Is BeamBCR.bookingId $ Se.Eq bookingId]

findByRideId :: (L.MonadFlow m, Log m) => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId (Id rideId) = findOneWithKV [Se.Is BeamBCR.rideId $ Se.Eq (Just rideId)]

findByRideIdInReplica :: (L.MonadFlow m, Log m) => Id Ride -> m (Maybe BookingCancellationReason)
findByRideIdInReplica (Id rideId) = findOneWithKvInReplica [Se.Is BeamBCR.rideId $ Se.Eq (Just rideId)]

upsert :: (L.MonadFlow m, Log m) => BookingCancellationReason -> m ()
upsert cancellationReason = do
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.reasonCode ((\(CancellationReasonCode x) -> x) <$> cancellationReason.reasonCode),
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason

-- findAllBookingIdsCancelledByDriverId :: Transactionable m => Id Person -> m [Id Booking]
-- findAllBookingIdsCancelledByDriverId driverId = do
--   Esq.findAll $ do
--     rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
--     where_ $
--       rideBookingCancellationReason ^. BookingCancellationReasonDriverId ==. val (Just $ toKey driverId)
--         &&. rideBookingCancellationReason ^. BookingCancellationReasonSource ==. val ByDriver
--     return (rideBookingCancellationReason ^. BookingCancellationReasonBookingId)

findAllBookingIdsCancelledByDriverId :: MonadFlow m => Id Person -> m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = findAllWithKV [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> (DBCR.bookingId <$>)

instance FromTType' (BeamBCR.BookingCancellationReasonT Identity) BookingCancellationReason where
  fromTType' BeamBCR.BookingCancellationReasonT {..} = do
    pure $
      Just
        BookingCancellationReason
          { driverId = Id <$> driverId,
            bookingId = Id bookingId,
            rideId = Id <$> rideId,
            merchantId = Id <$> merchantId,
            source = source,
            reasonCode = CancellationReasonCode <$> reasonCode,
            additionalInfo = additionalInfo,
            driverCancellationLocation = LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
            driverDistToPickup = driverDistToPickup
          }

instance ToTType' BeamBCR.BookingCancellationReason BookingCancellationReason where
  toTType' BookingCancellationReason {..} = do
    BeamBCR.BookingCancellationReasonT
      { BeamBCR.driverId = getId <$> driverId,
        BeamBCR.bookingId = getId bookingId,
        BeamBCR.rideId = getId <$> rideId,
        BeamBCR.merchantId = getId <$> merchantId,
        BeamBCR.source = source,
        BeamBCR.reasonCode = (\(CancellationReasonCode x) -> x) <$> reasonCode,
        BeamBCR.additionalInfo = additionalInfo,
        BeamBCR.driverCancellationLocationLat = lat <$> driverCancellationLocation,
        BeamBCR.driverCancellationLocationLon = lon <$> driverCancellationLocation,
        BeamBCR.driverDistToPickup = driverDistToPickup
      }
