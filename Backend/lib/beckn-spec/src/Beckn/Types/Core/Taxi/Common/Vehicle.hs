{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Vehicle where

import Kernel.Prelude

newtype Vehicle = Vehicle
  { category :: VehicleVariant
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS | BIKE | PREMIUM_SEDAN | BLACK | BLACK_XL | AMBULANCE_TAXI | AMBULANCE_TAXI_OXY | AMBULANCE_AC | AMBULANCE_AC_OXY | AMBULANCE_VENTILATOR | SUV_PLUS | DELIVERY_LIGHT_GOODS_VEHICLE | BUS_NON_AC | BUS_AC | HERITAGE_CAB | EV_AUTO_RICKSHAW | DELIVERY_TRUCK_MINI | DELIVERY_TRUCK_SMALL | DELIVERY_TRUCK_MEDIUM | DELIVERY_TRUCK_LARGE | DELIVERY_TRUCK_ULTRA_LARGE
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
