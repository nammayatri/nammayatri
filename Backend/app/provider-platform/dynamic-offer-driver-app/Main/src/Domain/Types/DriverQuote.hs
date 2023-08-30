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

import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Estimate
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver
import Domain.Types.SearchTry
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data DriverQuoteStatus = Active | Inactive
  deriving (Show, Read, Eq, Generic, Ord, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable DriverQuoteStatus

$(mkBeamInstancesForEnum ''DriverQuoteStatus)

data DriverQuote = DriverQuote
  { id :: Id DriverQuote,
    requestId :: Id SearchRequest,
    searchTryId :: Id SearchTry,
    searchRequestForDriverId :: Maybe (Id SearchRequestForDriver),
    estimateId :: Id Estimate,
    driverId :: Id Person,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    status :: DriverQuoteStatus,
    vehicleVariant :: Variant.Variant,
    distance :: Meters,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    validTill :: UTCTime,
    estimatedFare :: Money,
    fareParams :: Params.FareParameters,
    providerId :: Id DMerchant.Merchant,
    specialLocationTag :: Maybe Text,
    goHomeRequestId :: Maybe (Id DDGR.DriverGoHomeRequest),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow, Eq)
