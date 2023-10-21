{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import Domain.Types.FareParameters (FareParameters)
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

data BookingStatus
  = NEW
  | CONFIRMED
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

data BookingDetails
  = BookingDetailsOnDemand
      { specialZoneOtpCode :: Maybe Text,
        specialLocationTag :: Maybe Text,
        toLocation :: DLoc.Location
      }
  | BookingDetailsRental
      { rentalToLocation :: Maybe DLoc.Location
      }
  deriving (Generic)

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    quoteId :: Text,
    status :: BookingStatus,
    bookingType :: BookingType,
    bookingDetails :: BookingDetails,
    disabilityTag :: Maybe Text,
    area :: Maybe FareProductD.Area,
    providerId :: Id DM.Merchant, -- FIXME merchantId
    primaryExophone :: Text,
    estimatedFare :: Money,
    estimatedDistance :: Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedDuration :: Seconds,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Maybe Context.City,
    bapCountry :: Maybe Context.Country,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.Location,
    vehicleVariant :: DVeh.Variant,
    fareParams :: FareParameters,
    riderName :: Maybe Text,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    paymentUrl :: Maybe Text
  }
  deriving (Generic)

data BookingType = SpecialZoneBooking | NormalBooking | RentalBooking
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BookingType)
