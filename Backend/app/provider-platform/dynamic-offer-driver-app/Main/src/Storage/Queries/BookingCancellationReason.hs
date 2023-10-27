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

import qualified Data.List
import qualified Database.Beam as B
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
import Domain.Types.Ride
import qualified EulerHS.Language as L
import EulerHS.Prelude as P hiding (null, (^.))
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import qualified Storage.Beam.Common as BeamCommon

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DBCR.BookingCancellationReason -> m ()
create = createWithKV

findAllCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m Int
findAllCancelledByDriverId driverId = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            ( \bcr ->
                B.sqlBool_ (bcr.source B.==. B.val_ ByDriver)
                  B.&&?. (bcr.driverId B.==?. B.val_ (Just $ getId driverId))
            )
            do
              B.all_ (BeamCommon.bookingCancellationReason BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if Data.List.null r then 0 else Data.List.head r) res

findByBookingId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> m (Maybe BookingCancellationReason)
findByBookingId (Id bookingId) = findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq bookingId]

findByRideId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId (Id rideId) = findOneWithKV [Se.Is BeamBCR.rideId $ Se.Eq (Just rideId)]

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BookingCancellationReason -> m ()
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

findAllBookingIdsCancelledByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = findAllWithDb [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> (DBCR.bookingId <$>)

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
