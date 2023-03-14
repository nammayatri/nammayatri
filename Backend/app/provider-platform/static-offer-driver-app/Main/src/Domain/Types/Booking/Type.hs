{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.Type where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Servant.API

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Booking = Booking
  { id :: Id Booking,
    status :: BookingStatus,
    providerId :: Id DM.Merchant,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.BookingLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    reallocationsCount :: Int,
    bookingDetails :: BookingDetails,
    riderName :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON)

data BookingDetails = OneWayDetails OneWayBookingDetails | RentalDetails RentalBookingDetails
  deriving (Generic, Eq, ToJSON)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.BookingLocation,
    estimatedDistance :: Meters,
    estimatedFinishTime :: UTCTime,
    estimatedDuration :: Seconds
  }
  deriving (Eq, Generic, ToJSON)

newtype RentalBookingDetails = RentalBookingDetails
  { rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy
  }
  deriving (Eq, Generic, ToJSON)

mkRentalBookingDetails :: Id DRentalFP.RentalFarePolicy -> BookingDetails
mkRentalBookingDetails rentalFarePolicyId = RentalDetails $ RentalBookingDetails {..}

getFareProductType :: BookingDetails -> SFP.FareProductType
getFareProductType = \case
  OneWayDetails _ -> SFP.ONE_WAY
  RentalDetails _ -> SFP.RENTAL
