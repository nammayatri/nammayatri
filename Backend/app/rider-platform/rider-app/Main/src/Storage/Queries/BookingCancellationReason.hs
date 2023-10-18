{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.BookingCancellationReason where

import qualified Database.Beam as B
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import qualified Storage.Beam.Common as BeamCommon

create :: MonadFlow m => BookingCancellationReason -> m ()
create = createWithKV

findByRideBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe BookingCancellationReason)
findByRideBookingId (Id bookingId) = findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq bookingId]

upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => BookingCancellationReason -> m ()
upsert cancellationReason = do
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.source cancellationReason.source,
          Se.Set BeamBCR.reasonCode cancellationReason.reasonCode,
          Se.Set BeamBCR.reasonStage cancellationReason.reasonStage,
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason

countCancelledBookingsByBookingIds :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Id Booking] -> CancellationSource -> m Int
countCancelledBookingsByBookingIds bookingIds cancellationSource = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_
            (\BeamBCR.BookingCancellationReasonT {..} -> bookingId `B.in_` (B.val_ . getId <$> bookingIds) B.&&. (source B.==. B.val_ cancellationSource))
            do
              B.all_ (BeamCommon.bookingCancellationReason BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

instance FromTType' BeamBCR.BookingCancellationReason BookingCancellationReason where
  fromTType' BeamBCR.BookingCancellationReasonT {..} = do
    pure $
      Just
        BookingCancellationReason
          { bookingId = Id bookingId,
            rideId = Id <$> rideId,
            merchantId = Id <$> merchantId,
            source = source,
            reasonCode = reasonCode,
            reasonStage = reasonStage,
            additionalInfo = additionalInfo,
            driverCancellationLocation = LatLong <$> driverCancellationLocationLat <*> driverCancellationLocationLon,
            driverDistToPickup = driverDistToPickup
          }

instance ToTType' BeamBCR.BookingCancellationReason BookingCancellationReason where
  toTType' BookingCancellationReason {..} = do
    BeamBCR.BookingCancellationReasonT
      { BeamBCR.bookingId = getId bookingId,
        BeamBCR.rideId = getId <$> rideId,
        BeamBCR.merchantId = getId <$> merchantId,
        BeamBCR.source = source,
        BeamBCR.reasonStage = reasonStage,
        BeamBCR.reasonCode = reasonCode,
        BeamBCR.additionalInfo = additionalInfo,
        BeamBCR.driverCancellationLocationLat = driverCancellationLocation <&> (.lat),
        BeamBCR.driverCancellationLocationLon = driverCancellationLocation <&> (.lon),
        BeamBCR.driverDistToPickup = driverDistToPickup
      }
