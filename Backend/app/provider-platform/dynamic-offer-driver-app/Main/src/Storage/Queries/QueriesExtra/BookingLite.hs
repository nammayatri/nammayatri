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
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions)
          }
