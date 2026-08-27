{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.BookingLite where

import qualified Domain.Types.Booking
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderDetails
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Kernel.Utils.JSON
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as Beam
import qualified Storage.Queries.Transformers.Booking

---------------- Use this function if you need the data which are here as per your domain requirement ----------------
---------------- Add items in the below domain according to your use                                  ----------------
---------------- NOTE: keep this lightweight — only scalar columns / pure transforms, NO extra table  ----------------
---------------- reads (LocationMapping / Location / FareParameters). It must stay a single KV read.  ----------------

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe BookingLite))
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBookingsFromDBLite :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Types.Id.Id BookingLite] -> m [BookingLite]
findBookingsFromDBLite bookingIds = findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> bookingIds)]

-- Lite list read for the ops "scheduled bookings" dashboard: scalar columns only, NO
-- LocationMapping/Location joins (pickup is resolved per-page in the handler).
findScheduledUpcomingBookingsLite ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  [Domain.Types.Booking.BookingStatus] ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  m [BookingLite]
findScheduledUpcomingBookingsLite merchantOpCityId statuses fromTime toTime limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just (Kernel.Types.Id.getId merchantOpCityId)),
          Se.Is Beam.isScheduled $ Se.Eq (Just True),
          Se.Is Beam.status $ Se.In statuses,
          Se.Is Beam.startTime $ Se.GreaterThanOrEq fromTime,
          Se.Is Beam.startTime $ Se.LessThanOrEq toTime
        ]
    ]
    (Se.Asc Beam.startTime)
    (Just limit)
    (Just offset)

-- All bookings sharing a transactionId, lite (for reallocation-count without location joins).
findAllByTransactionIdLite :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [BookingLite]
findAllByTransactionIdLite txnId = findAllWithKV [Se.Is Beam.transactionId $ Se.Eq txnId]

data BookingLite = BookingLite
  { id :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    riderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.Booking.BookingStatus,
    transactionId :: Kernel.Prelude.Text,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    quoteId :: Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    displayBookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails),
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    tripCategory :: Domain.Types.Common.TripCategory,
    startTime :: Kernel.Prelude.UTCTime,
    isScheduled :: Kernel.Prelude.Bool,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type BookingLiteTable = Beam.BookingT Identity

instance FromTType' BookingLiteTable BookingLite where
  fromTType' (Beam.BookingT {..}) = do
    pure $
      Just
        BookingLite
          { id = Kernel.Types.Id.Id id,
            riderName = riderName,
            status = status,
            transactionId = transactionId,
            providerId = Kernel.Types.Id.Id providerId,
            quoteId = quoteId,
            bapId = bapId,
            displayBookingId = displayBookingId,
            riderId = Kernel.Types.Id.Id <$> riderId,
            vehicleServiceTier = vehicleVariant,
            tripCategory = Storage.Queries.Transformers.Booking.getTripCategory bookingType tripCategory,
            startTime = startTime,
            isScheduled = fromMaybe False isScheduled,
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions)
          }
