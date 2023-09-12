{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Booking where

import qualified Database.Beam as B
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Domain.BookingStatus,
    bookingType :: B.C f Domain.BookingType,
    specialLocationTag :: B.C f (Maybe Text),
    specialZoneOtpCode :: B.C f (Maybe Text),
    disabilityTag :: B.C f (Maybe Text),
    area :: B.C f (Maybe FareProductD.Area),
    providerId :: B.C f Text,
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    bapCity :: B.C f (Maybe Context.City),
    bapCountry :: B.C f (Maybe Context.Country),
    startTime :: B.C f UTCTime,
    riderId :: B.C f (Maybe Text),
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f Meters,
    maxEstimatedDistance :: B.C f (Maybe HighPrecMeters),
    estimatedFare :: B.C f Money,
    estimatedDuration :: B.C f Seconds,
    fareParametersId :: B.C f Text,
    riderName :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    paymentMethodId :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['specialZoneOtpCode], ['quoteId]])

$(mkTableInstances ''BookingT "booking")
