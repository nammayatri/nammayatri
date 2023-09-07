{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Booking where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Booking.Type as Domain
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant (..))
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    fulfillmentId :: B.C f (Maybe Text),
    driverId :: B.C f (Maybe Text),
    itemId :: B.C f Text,
    fareProductType :: B.C f DQuote.FareProductType,
    bppBookingId :: B.C f (Maybe Text),
    quoteId :: B.C f (Maybe Text),
    riderId :: B.C f Text,
    paymentMethodId :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    status :: B.C f Domain.BookingStatus,
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    primaryExophone :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f (Maybe Text),
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    distance :: B.C f (Maybe HighPrecMeters),
    otpCode :: B.C f (Maybe Text),
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    tripTermsId :: B.C f (Maybe Text),
    rentalSlabId :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['bppBookingId], ['riderId], ['quoteId]])

$(mkTableInstancesWithTModifier ''BookingT "booking" [("bppBookingId", "bpp_ride_booking_id")])
