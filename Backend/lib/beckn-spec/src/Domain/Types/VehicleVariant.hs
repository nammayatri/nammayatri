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

import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.List as DL
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
  | BOAT
  | VIP_ESCORT
  | VIP_OFFICER
  | AC_PRIORITY
  | BIKE_PLUS
  | E_RICKSHAW
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded, EP.Hashable)

instance CH.ClickhouseValue VehicleVariant

allVehicleVariants :: [VehicleVariant]
allVehicleVariants = [minBound .. maxBound]

$(mkHttpInstancesForEnum ''VehicleVariant)

data VariantMetadata = VariantMetadata
  { vehicleCategory :: DVC.VehicleCategory,
    becknCategory :: Enums.VehicleCategory,
    becknVariantString :: Text,
    avgSpeedFieldName :: Text,
    serviceTierType :: Maybe DVST.ServiceTierType
  }
  deriving (Show, Eq)

serviceTierSpecialCases :: [(DVST.ServiceTierType, VehicleVariant)]
serviceTierSpecialCases =
  [ (DVST.ECO, HATCHBACK),
    (DVST.COMFY, SEDAN),
    (DVST.PREMIUM, SEDAN)
  ]

variantMetadataMap :: [(VehicleVariant, VariantMetadata)]
variantMetadataMap =
  [ (SEDAN, VariantMetadata DVC.CAR Enums.CAB "SEDAN" "sedan" (Just DVST.SEDAN)),
    (SUV, VariantMetadata DVC.CAR Enums.CAB "SUV" "suv" (Just DVST.SUV)),
    (HATCHBACK, VariantMetadata DVC.CAR Enums.CAB "HATCHBACK" "hatchback" (Just DVST.HATCHBACK)),
    (AUTO_RICKSHAW, VariantMetadata DVC.AUTO_CATEGORY Enums.AUTO_RICKSHAW "AUTO_RICKSHAW" "autorickshaw" (Just DVST.AUTO_RICKSHAW)),
    (AUTO_PLUS, VariantMetadata DVC.AUTO_CATEGORY Enums.AUTO_RICKSHAW "AUTO_PLUS" "autorickshaw" (Just DVST.AUTO_PLUS)),
    (TAXI, VariantMetadata DVC.CAR Enums.CAB "TAXI" "taxi" (Just DVST.TAXI)),
    (TAXI_PLUS, VariantMetadata DVC.CAR Enums.CAB "TAXI_PLUS" "taxiplus" (Just DVST.TAXI_PLUS)),
    (PREMIUM_SEDAN, VariantMetadata DVC.CAR Enums.CAB "PREMIUM_SEDAN" "premiumsedan" (Just DVST.PREMIUM_SEDAN)),
    (BLACK, VariantMetadata DVC.CAR Enums.CAB "BLACK" "black" (Just DVST.BLACK)),
    (BLACK_XL, VariantMetadata DVC.CAR Enums.CAB "BLACK_XL" "blackxl" (Just DVST.BLACK_XL)),
    (BIKE, VariantMetadata DVC.MOTORCYCLE Enums.MOTORCYCLE "BIKE" "bike" (Just DVST.BIKE)),
    (AMBULANCE_TAXI, VariantMetadata DVC.AMBULANCE Enums.AMBULANCE "AMBULANCE_TAXI" "ambulance" (Just DVST.AMBULANCE_TAXI)),
    (AMBULANCE_TAXI_OXY, VariantMetadata DVC.AMBULANCE Enums.AMBULANCE "AMBULANCE_TAXI_OXY" "ambulance" (Just DVST.AMBULANCE_TAXI_OXY)),
    (AMBULANCE_AC, VariantMetadata DVC.AMBULANCE Enums.AMBULANCE "AMBULANCE_AC" "ambulance" (Just DVST.AMBULANCE_AC)),
    (AMBULANCE_AC_OXY, VariantMetadata DVC.AMBULANCE Enums.AMBULANCE "AMBULANCE_AC_OXY" "ambulance" (Just DVST.AMBULANCE_AC_OXY)),
    (AMBULANCE_VENTILATOR, VariantMetadata DVC.AMBULANCE Enums.AMBULANCE "AMBULANCE_VENTILATOR" "ambulance" (Just DVST.AMBULANCE_VENTILATOR)),
    (SUV_PLUS, VariantMetadata DVC.CAR Enums.CAB "SUV_PLUS" "suvplus" (Just DVST.SUV_PLUS)),
    (DELIVERY_BIKE, VariantMetadata DVC.MOTORCYCLE Enums.MOTORCYCLE "DELIVERY_BIKE" "bike" (Just DVST.DELIVERY_BIKE)),
    (DELIVERY_LIGHT_GOODS_VEHICLE, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_LIGHT_GOODS_VEHICLE" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_LIGHT_GOODS_VEHICLE)),
    (DELIVERY_TRUCK_MINI, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_TRUCK_MINI" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_TRUCK_MINI)),
    (DELIVERY_TRUCK_SMALL, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_TRUCK_SMALL" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_TRUCK_SMALL)),
    (DELIVERY_TRUCK_MEDIUM, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_TRUCK_MEDIUM" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_TRUCK_MEDIUM)),
    (DELIVERY_TRUCK_LARGE, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_TRUCK_LARGE" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_TRUCK_LARGE)),
    (DELIVERY_TRUCK_ULTRA_LARGE, VariantMetadata DVC.TRUCK Enums.TRUCK "DELIVERY_TRUCK_ULTRA_LARGE" "deliveryLightGoodsVehicle" (Just DVST.DELIVERY_TRUCK_ULTRA_LARGE)),
    (BUS_NON_AC, VariantMetadata DVC.BUS Enums.BUS "BUS_NON_AC" "busNonAc" (Just DVST.BUS_NON_AC)),
    (BUS_AC, VariantMetadata DVC.BUS Enums.BUS "BUS_AC" "busAc" (Just DVST.BUS_AC)),
    (HERITAGE_CAB, VariantMetadata DVC.CAR Enums.CAB "HERITAGE_CAB" "heritagecab" (Just DVST.HERITAGE_CAB)),
    (EV_AUTO_RICKSHAW, VariantMetadata DVC.AUTO_CATEGORY Enums.AUTO_RICKSHAW "EV_AUTO_RICKSHAW" "evautorickshaw" (Just DVST.EV_AUTO_RICKSHAW)),
    (BOAT, VariantMetadata DVC.BOAT Enums.BOAT "BOAT" "boat" (Just DVST.BOAT)),
    (VIP_ESCORT, VariantMetadata DVC.CAR Enums.CAB "VIP_ESCORT" "vipEscort" (Just DVST.VIP_ESCORT)),
    (VIP_OFFICER, VariantMetadata DVC.CAR Enums.CAB "VIP_OFFICER" "vipOfficer" (Just DVST.VIP_OFFICER)),
    (AC_PRIORITY, VariantMetadata DVC.CAR Enums.CAB "AC_PRIORITY" "sedan" (Just DVST.AC_PRIORITY)),
    (BIKE_PLUS, VariantMetadata DVC.MOTORCYCLE Enums.MOTORCYCLE "BIKE_PLUS" "bikeplus" (Just DVST.BIKE_PLUS)),
    (E_RICKSHAW, VariantMetadata DVC.AUTO_CATEGORY Enums.AUTO_RICKSHAW "E_RICKSHAW" "erickshaw" (Just DVST.E_RICKSHAW))
  ]

getVariantMetadata :: VehicleVariant -> Maybe VariantMetadata
getVariantMetadata variant = lookup variant variantMetadataMap

getVehicleCategoryForVariant :: VehicleVariant -> DVC.VehicleCategory
getVehicleCategoryForVariant variant = maybe DVC.AUTO_CATEGORY vehicleCategory (getVariantMetadata variant)

getRawBecknCategoryForVariant :: VehicleVariant -> Enums.VehicleCategory -- Returns MOTORCYCLE for bike variants, not TWO_WHEELER (can be deprecarted I think but created for safety)
getRawBecknCategoryForVariant variant = maybe Enums.CAB becknCategory (getVariantMetadata variant)

getBecknCategoryForVariant :: VehicleVariant -> Enums.VehicleCategory -- Converts MOTORCYCLE to TWO_WHEELER for API compatibility :)
getBecknCategoryForVariant variant = case getRawBecknCategoryForVariant variant of
  Enums.MOTORCYCLE -> Enums.TWO_WHEELER
  cat -> cat

getBecknVariantString :: VehicleVariant -> Text
getBecknVariantString variant = maybe (show variant) becknVariantString (getVariantMetadata variant)

castVariantToBeckn :: VehicleVariant -> (Text, Text)
castVariantToBeckn variant = (show $ getBecknCategoryForVariant variant, getBecknVariantString variant)

castServiceTierToVariant :: DVST.ServiceTierType -> VehicleVariant
castServiceTierToVariant serviceTier = case lookup serviceTier serviceTierSpecialCases of
  Just variant -> variant
  Nothing -> case DL.find (\(_, metadata) -> metadata.serviceTierType == Just serviceTier) variantMetadataMap of
    Just (variant, _) -> variant
    Nothing -> SEDAN -- TODO: Handle properly

castVariantToServiceTier :: VehicleVariant -> DVST.ServiceTierType
castVariantToServiceTier variant = case getVariantMetadata variant >>= serviceTierType of
  Just serviceTier -> serviceTier
  Nothing -> DVST.SEDAN -- TODO: Handle properly

castServiceTierToVehicleCategory :: DVST.ServiceTierType -> DVC.VehicleCategory
castServiceTierToVehicleCategory serviceTier = getVehicleCategoryForVariant $ castServiceTierToVariant serviceTier

parseVehicleVariantFromBeckn :: Maybe Text -> Maybe Text -> Maybe VehicleVariant
parseVehicleVariantFromBeckn mbCategory mbVariant =
  let normalizedCategory = case mbCategory of
        Just "MOTORCYCLE" -> Just "TWO_WHEELER"
        Just "TWO_WHEELER" -> Just "TWO_WHEELER"
        cat -> cat
   in case (normalizedCategory, mbVariant) of
        (Just catStr, Just varStr) ->
          DL.find
            ( \(variant, _) ->
                show (getBecknCategoryForVariant variant) == catStr
                  && getBecknVariantString variant == varStr
            )
            variantMetadataMap
            <&> fst
        _ -> Nothing

castVehicleVariantToVehicleCategory :: VehicleVariant -> DVC.VehicleCategory
castVehicleVariantToVehicleCategory = getVehicleCategoryForVariant

getVehicleCategory :: Maybe DVC.VehicleCategory -> VehicleVariant -> Maybe DVC.VehicleCategory
getVehicleCategory mbVehicleCategory variant = mbVehicleCategory <|> (Just $ castVehicleVariantToVehicleCategory variant)

getVehicleCategoryFromVehicleVariantDefault :: Maybe VehicleVariant -> DVC.VehicleCategory
getVehicleCategoryFromVehicleVariantDefault = maybe DVC.AUTO_CATEGORY castVehicleVariantToVehicleCategory -- handle default properly

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
