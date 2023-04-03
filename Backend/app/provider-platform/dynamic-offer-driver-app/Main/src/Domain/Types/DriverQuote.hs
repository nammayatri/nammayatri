{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.DriverQuote where

import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver
import Domain.Types.SearchStep
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read, Eq)
  deriving (PrettyShow) via Showable DriverQuoteStatus

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    status :: DriverQuoteStatus,
    requestId :: Id SearchRequest,
    searchStepId :: Id SearchStep,
    searchRequestForDriverId :: Maybe (Id SearchRequestForDriver), -- TODO :: Remove Maybe
    driverId :: Id Person,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    vehicleVariant :: Variant.Variant,
    distance :: Meters,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    validTill :: UTCTime,
    estimatedFare :: Money,
    fareParams :: Params.FareParameters,
    providerId :: Id DMerchant.Merchant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow)
