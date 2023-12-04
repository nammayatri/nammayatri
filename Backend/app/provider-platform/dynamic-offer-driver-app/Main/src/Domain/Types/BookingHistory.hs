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
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

data BookingHistory = BookingHistory
  { id :: Id BookingHistory,
    transactionId :: Text,
    quoteId :: Text,
    status :: BookingStatus,
    bookingType :: BookingType,
    specialZoneOtpCode :: Maybe Text,
    specialLocationTag :: Maybe Text,
    disabilityTag :: Maybe Text,
    area :: Maybe FareProductD.Area,
    providerId :: Id DM.Merchant, -- FIXME merchantId
    primaryExophone :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Maybe Context.City,
    bapCountry :: Maybe Context.Country,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.Location,
    toLocation :: DLoc.Location,
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedFare :: Money,
    estimatedDuration :: Seconds,
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    serviceCharge :: Maybe Money,
    govtCharges :: Maybe Money,
    baseFare :: Money,
    waitingCharge :: Maybe Money,
    nightShiftCharge :: Maybe Money,
    nightShiftRateIfApplies :: Maybe Double,
    fareParametersDetails :: FareParametersDetails,
    riderName :: Maybe Text,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    paymentUrl :: Maybe Text
  }
  deriving (Generic)

data BookingType = SpecialZoneBooking | NormalBooking
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BookingType)

data BookingHistorysDetails = ProgressiveDetails BHistoryProgressiveDetails | SlabDetails BHistorySlabDetails
  deriving (Generic, Show, Eq, PrettyShow)

data BHistoryProgressiveDetails = BHistoryProgressiveDetails
  { deadKmFare :: Money,
    extraKmFare :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)

data BHistorySlabDetails = BHistorySlabDetails
  { platformFee :: Maybe Money,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow)

type FullBookingHistoryProgressiveDetails = (Id BookingHistory, BHistoryProgressiveDetails)

data BookingHistoryType = Progressive | Slab
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FareParametersType)

getBookingHistoryType :: FareParameters -> FareParametersType
getBookingHistoryType bookingHistory = case bookingHistory.bookingHistoryDetails of
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
