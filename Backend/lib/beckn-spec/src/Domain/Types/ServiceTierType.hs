{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.ServiceTierType where

import Data.Aeson
import Data.OpenApi hiding (name)
import EulerHS.Prelude hiding (length, map, readMaybe)
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data ServiceTierType
  = COMFY
  | ECO
  | PREMIUM
  | SUV
  | AUTO_RICKSHAW
  | HATCHBACK
  | SEDAN
  | TAXI
  | TAXI_PLUS
  | PREMIUM_SEDAN
  | BLACK
  | BLACK_XL
  | BIKE
  | AMBULANCE_TAXI
  | AMBULANCE_TAXI_OXY
  | AMBULANCE_AC
  | AMBULANCE_AC_OXY
  | AMBULANCE_VENTILATOR
  | SUV_PLUS
  | DELIVERY_BIKE
  | DELIVERY_LIGHT_GOODS_VEHICLE
  | DELIVERY_TRUCK_MINI
  | DELIVERY_TRUCK_SMALL
  | DELIVERY_TRUCK_MEDIUM
  | DELIVERY_TRUCK_LARGE
  | DELIVERY_TRUCK_ULTRA_LARGE
  | BUS_NON_AC
  | BUS_AC
  | HERITAGE_CAB
  | EV_AUTO_RICKSHAW
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, EulerHS.Prelude.Hashable, Enum, Bounded)

$(mkHttpInstancesForEnum ''ServiceTierType)

instance CH.ClickhouseValue ServiceTierType

offUsVariants :: [ServiceTierType]
offUsVariants = [AUTO_RICKSHAW, SEDAN, HATCHBACK, SUV]
