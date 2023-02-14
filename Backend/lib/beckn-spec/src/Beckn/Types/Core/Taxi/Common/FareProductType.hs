 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.FareProductType where

import Kernel.Prelude hiding (show)

-- TODO
-- ONE_WAY_TRIP, RENTAL_TRIP, DRIVER_OFFER_ESTIMATE can be used in on_search,
-- only DRIVER_OFFER can be used in on_select,
-- ONE_WAY_TRIP, RENTAL_TRIP, DRIVER_OFFER can be used in init
-- we can express it in specific types for each endpoint
data FareProductType
  = ONE_WAY_TRIP
  | RENTAL_TRIP
  | DRIVER_OFFER_ESTIMATE
  | DRIVER_OFFER
  deriving
    ( Eq,
      Ord,
      Generic,
      ToSchema,
      Show,
      FromJSON,
      ToJSON,
      Read
    )
