{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.QuoteRental where

import qualified Domain.Types.FareParameters as Params
import Domain.Types.FarePolicy
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data QuoteRental = QuoteRental
  { id :: Id QuoteRental,
    searchRequestId :: Id SearchRequest,
    farePolicyId :: Id FarePolicy,
    providerId :: Id DMerchant.Merchant,
    vehicleVariant :: Variant.Variant,
    estimatedFinishTime :: UTCTime,
    validTill :: UTCTime,
    baseDistance :: Meters,
    baseDuration :: Seconds,
    baseFare :: Money,
    fareParams :: Params.FareParameters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
