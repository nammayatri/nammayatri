 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Quote where

import Data.Time
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra
import Kernel.Types.Common
import Kernel.Types.Id

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    providerId :: Id DM.Merchant,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime,
    quoteDetails :: QuoteDetails
  }

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails DRentalFP.RentalFarePolicy

data OneWayQuoteDetails = OneWayQuoteDetails
  { distance :: Meters,
    distanceToNearestDriver :: Meters,
    estimatedFinishTime :: UTCTime
  }

getFareProductType :: QuoteDetails -> DFareProduct.FareProductType
getFareProductType = \case
  OneWayDetails _ -> DFareProduct.ONE_WAY
  RentalDetails _ -> DFareProduct.RENTAL
