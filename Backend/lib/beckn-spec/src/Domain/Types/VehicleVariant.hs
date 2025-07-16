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

module Domain.Types.VehicleVariant where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleCategory as DVC
import qualified EulerHS.Prelude as EP
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data VehicleVariant
  = SEDAN
  | SUV
  | HATCHBACK
  | AUTO_RICKSHAW
  | AUTO_PLUS
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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded, EP.Hashable)

instance CH.ClickhouseValue VehicleVariant

allVehicleVariants :: [VehicleVariant]
allVehicleVariants = [minBound .. maxBound]

$(mkHttpInstancesForEnum ''VehicleVariant)

castServiceTierToVariant :: DVST.ServiceTierType -> VehicleVariant
castServiceTierToVariant = \case
  DVST.SEDAN -> SEDAN
  DVST.ECO -> SEDAN
  DVST.COMFY -> SEDAN
  DVST.PREMIUM -> SEDAN
  DVST.HATCHBACK -> HATCHBACK
  DVST.TAXI -> TAXI
  DVST.SUV -> SUV
  DVST.TAXI_PLUS -> TAXI_PLUS
  DVST.AUTO_RICKSHAW -> AUTO_RICKSHAW
  DVST.EV_AUTO_RICKSHAW -> EV_AUTO_RICKSHAW
  DVST.PREMIUM_SEDAN -> PREMIUM_SEDAN
  DVST.BLACK -> BLACK
  DVST.BLACK_XL -> BLACK_XL
  DVST.BIKE -> BIKE
  DVST.AMBULANCE_TAXI -> AMBULANCE_TAXI
  DVST.AMBULANCE_TAXI_OXY -> AMBULANCE_TAXI_OXY
  DVST.AMBULANCE_AC -> AMBULANCE_AC
  DVST.AMBULANCE_AC_OXY -> AMBULANCE_AC_OXY
  DVST.AMBULANCE_VENTILATOR -> AMBULANCE_VENTILATOR
  DVST.SUV_PLUS -> SUV_PLUS
  DVST.HERITAGE_CAB -> HERITAGE_CAB
  DVST.DELIVERY_BIKE -> DELIVERY_BIKE
  DVST.DELIVERY_LIGHT_GOODS_VEHICLE -> DELIVERY_LIGHT_GOODS_VEHICLE
  DVST.DELIVERY_TRUCK_MINI -> DELIVERY_TRUCK_MINI
  DVST.DELIVERY_TRUCK_SMALL -> DELIVERY_TRUCK_SMALL
  DVST.DELIVERY_TRUCK_MEDIUM -> DELIVERY_TRUCK_MEDIUM
  DVST.DELIVERY_TRUCK_LARGE -> DELIVERY_TRUCK_LARGE
  DVST.DELIVERY_TRUCK_ULTRA_LARGE -> DELIVERY_TRUCK_ULTRA_LARGE
  DVST.BUS_NON_AC -> BUS_NON_AC
  DVST.BUS_AC -> BUS_AC
  DVST.AUTO_PLUS -> AUTO_PLUS

castVariantToServiceTier :: VehicleVariant -> DVST.ServiceTierType
castVariantToServiceTier = \case
  SEDAN -> DVST.SEDAN
  HATCHBACK -> DVST.HATCHBACK
  TAXI -> DVST.TAXI
  SUV -> DVST.SUV
  TAXI_PLUS -> DVST.TAXI_PLUS
  PREMIUM_SEDAN -> DVST.PREMIUM_SEDAN
  BLACK -> DVST.BLACK
  BLACK_XL -> DVST.BLACK_XL
  AUTO_RICKSHAW -> DVST.AUTO_RICKSHAW
  BIKE -> DVST.BIKE
  AMBULANCE_TAXI -> DVST.AMBULANCE_TAXI
  AMBULANCE_TAXI_OXY -> DVST.AMBULANCE_TAXI_OXY
  AMBULANCE_AC -> DVST.AMBULANCE_AC
  AMBULANCE_AC_OXY -> DVST.AMBULANCE_AC_OXY
  AMBULANCE_VENTILATOR -> DVST.AMBULANCE_VENTILATOR
  SUV_PLUS -> DVST.SUV_PLUS
  EV_AUTO_RICKSHAW -> DVST.EV_AUTO_RICKSHAW
  HERITAGE_CAB -> DVST.HERITAGE_CAB
  DELIVERY_BIKE -> DVST.DELIVERY_BIKE
  DELIVERY_LIGHT_GOODS_VEHICLE -> DVST.DELIVERY_LIGHT_GOODS_VEHICLE
  DELIVERY_TRUCK_MINI -> DVST.DELIVERY_TRUCK_MINI
  DELIVERY_TRUCK_SMALL -> DVST.DELIVERY_TRUCK_SMALL
  DELIVERY_TRUCK_MEDIUM -> DVST.DELIVERY_TRUCK_MEDIUM
  DELIVERY_TRUCK_LARGE -> DVST.DELIVERY_TRUCK_LARGE
  DELIVERY_TRUCK_ULTRA_LARGE -> DVST.DELIVERY_TRUCK_ULTRA_LARGE
  BUS_NON_AC -> DVST.BUS_NON_AC
  BUS_AC -> DVST.BUS_AC
  AUTO_PLUS -> DVST.AUTO_PLUS

castVehicleVariantToVehicleCategory :: VehicleVariant -> DVC.VehicleCategory
castVehicleVariantToVehicleCategory = \case
  SEDAN -> DVC.CAR
  SUV -> DVC.CAR
  HATCHBACK -> DVC.CAR
  AUTO_RICKSHAW -> DVC.AUTO_CATEGORY
  TAXI -> DVC.CAR
  TAXI_PLUS -> DVC.CAR
  PREMIUM_SEDAN -> DVC.CAR
  BLACK -> DVC.CAR
  BLACK_XL -> DVC.CAR
  BIKE -> DVC.MOTORCYCLE
  AMBULANCE_TAXI -> DVC.AMBULANCE
  AMBULANCE_TAXI_OXY -> DVC.AMBULANCE
  AMBULANCE_AC -> DVC.AMBULANCE
  AMBULANCE_AC_OXY -> DVC.AMBULANCE
  AMBULANCE_VENTILATOR -> DVC.AMBULANCE
  SUV_PLUS -> DVC.CAR
  EV_AUTO_RICKSHAW -> DVC.AUTO_CATEGORY
  HERITAGE_CAB -> DVC.CAR
  DELIVERY_BIKE -> DVC.MOTORCYCLE
  DELIVERY_LIGHT_GOODS_VEHICLE -> DVC.TRUCK
  DELIVERY_TRUCK_MINI -> DVC.TRUCK
  DELIVERY_TRUCK_SMALL -> DVC.TRUCK
  DELIVERY_TRUCK_MEDIUM -> DVC.TRUCK
  DELIVERY_TRUCK_LARGE -> DVC.TRUCK
  DELIVERY_TRUCK_ULTRA_LARGE -> DVC.TRUCK
  BUS_NON_AC -> DVC.BUS
  BUS_AC -> DVC.BUS
  AUTO_PLUS -> DVC.AUTO_CATEGORY

castServiceTierToVehicleCategory :: DVST.ServiceTierType -> DVC.VehicleCategory
castServiceTierToVehicleCategory = \case
  DVST.SEDAN -> DVC.CAR
  DVST.ECO -> DVC.CAR
  DVST.COMFY -> DVC.CAR
  DVST.PREMIUM -> DVC.CAR
  DVST.HATCHBACK -> DVC.CAR
  DVST.TAXI -> DVC.CAR
  DVST.SUV -> DVC.CAR
  DVST.TAXI_PLUS -> DVC.CAR
  DVST.AUTO_RICKSHAW -> DVC.AUTO_CATEGORY
  DVST.PREMIUM_SEDAN -> DVC.CAR
  DVST.BLACK -> DVC.CAR
  DVST.BLACK_XL -> DVC.CAR
  DVST.BIKE -> DVC.MOTORCYCLE
  DVST.AMBULANCE_TAXI -> DVC.AMBULANCE
  DVST.AMBULANCE_TAXI_OXY -> DVC.AMBULANCE
  DVST.AMBULANCE_AC -> DVC.AMBULANCE
  DVST.AMBULANCE_AC_OXY -> DVC.AMBULANCE
  DVST.AMBULANCE_VENTILATOR -> DVC.AMBULANCE
  DVST.SUV_PLUS -> DVC.CAR
  DVST.EV_AUTO_RICKSHAW -> DVC.AUTO_CATEGORY
  DVST.HERITAGE_CAB -> DVC.CAR
  DVST.DELIVERY_BIKE -> DVC.MOTORCYCLE
  DVST.DELIVERY_LIGHT_GOODS_VEHICLE -> DVC.TRUCK
  DVST.DELIVERY_TRUCK_MINI -> DVC.TRUCK
  DVST.DELIVERY_TRUCK_SMALL -> DVC.TRUCK
  DVST.DELIVERY_TRUCK_MEDIUM -> DVC.TRUCK
  DVST.DELIVERY_TRUCK_LARGE -> DVC.TRUCK
  DVST.DELIVERY_TRUCK_ULTRA_LARGE -> DVC.TRUCK
  DVST.BUS_NON_AC -> DVC.BUS
  DVST.BUS_AC -> DVC.BUS
  DVST.AUTO_PLUS -> DVC.AUTO_CATEGORY

getVehicleCategory :: Maybe DVC.VehicleCategory -> VehicleVariant -> Maybe DVC.VehicleCategory
getVehicleCategory mbVehicleCategory variant = mbVehicleCategory <|> (Just $ castVehicleVariantToVehicleCategory variant)

getVehicleCategoryFromVehicleVariantDefault :: Maybe VehicleVariant -> DVC.VehicleCategory
getVehicleCategoryFromVehicleVariantDefault = maybe defaultCategory castVehicleVariantToVehicleCategory
  where
    defaultCategory = DVC.AUTO_CATEGORY

getTruckVehicleVariant :: Maybe Float -> Maybe Float -> VehicleVariant -> VehicleVariant
getTruckVehicleVariant mbGrossVehicleWeight mbUnladdenWeight currentVariant = flip (maybe currentVariant) ((,) <$> mbGrossVehicleWeight <*> mbUnladdenWeight) $
  \(grossVehicleWeight, unladdenWeight) -> getVariantBasedOnWeight (grossVehicleWeight - unladdenWeight)
  where
    getVariantBasedOnWeight weight
      | weight > 4000 = DELIVERY_LIGHT_GOODS_VEHICLE
      | weight >= 2500 = DELIVERY_TRUCK_ULTRA_LARGE
      | weight >= 1500 = DELIVERY_TRUCK_LARGE
      | weight >= 1000 = DELIVERY_TRUCK_MEDIUM
      | weight >= 500 = DELIVERY_TRUCK_SMALL
      | weight >= 350 = DELIVERY_TRUCK_MINI
      | otherwise = DELIVERY_LIGHT_GOODS_VEHICLE
