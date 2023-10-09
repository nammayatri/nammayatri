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
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.SearchRequestRental
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data QuoteRental = QuoteRental
  { id :: Id QuoteRental,
    searchRequestId :: Id SearchRequestRental,
    providerId :: Id DMerchant.Merchant,
    vehicleVariant :: Variant.Variant,
    distance :: Kilometers,
    estimatedFinishTime :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    validTill :: UTCTime,
    estimatedFare :: Money,
    specialLocationTag :: Maybe Text,
    fareParams :: Params.FareParameters
  }
  deriving (Generic, Show)
