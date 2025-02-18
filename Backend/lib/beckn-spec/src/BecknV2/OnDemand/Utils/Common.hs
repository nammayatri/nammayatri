{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module BecknV2.OnDemand.Utils.Common where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Data.Data (Data, gmapQ)
import Data.Generics.Aliases (ext1Q)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Domain.Types
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DTV
import EulerHS.Prelude
import Kernel.Prelude (intToNominalDiffTime, listToMaybe)
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Error
import Kernel.Types.TimeRFC339 (convertRFC3339ToUTC)
import Kernel.Utils.Common
import Text.Printf (printf)

allNothing :: (Data d) => d -> Bool
allNothing = not . or . gmapQ (const True `ext1Q` isJust)

getStartLocation :: [Spec.Stop] -> Maybe Spec.Stop
getStartLocation = find (\stop -> stop.stopType == Just (show Enums.START))

getDropLocation :: [Spec.Stop] -> Maybe Spec.Stop
getDropLocation = find (\stop -> stop.stopType == Just (show Enums.END))

getTransactionId :: (MonadFlow m) => Spec.Context -> m Text
getTransactionId context = context.contextTransactionId <&> UUID.toText & fromMaybeM (InvalidRequest "Transaction Id not found")

getDriverNumber :: (MonadFlow m) => Spec.Order -> m Text
getDriverNumber message = message.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & fromMaybeM (InvalidRequest "driverMobileNumber is not present in RideAssigned Event.")

getMessageId :: (MonadFlow m) => Spec.Context -> m Text
getMessageId context = context.contextMessageId <&> UUID.toText & fromMaybeM (InvalidRequest "Transaction Id not found")

gpsToText :: Gps.Gps -> Maybe Text
gpsToText Gps.Gps {..} = Just $ T.pack (printf "%.6f" lat) <> ", " <> T.pack (printf "%.6f" lon)

getTimestampAndValidTill :: (MonadFlow m, Log m) => Spec.Context -> m (UTCTime, UTCTime)
getTimestampAndValidTill context = do
  ttl <- context.contextTtl >>= Utils.parseISO8601Duration & fromMaybeM (InvalidRequest "Missing ttl")
  timestamp <- context.contextTimestamp >>= Just . convertRFC3339ToUTC & fromMaybeM (InvalidRequest "Missing timestamp")
  return (timestamp, addUTCTime ttl timestamp)

computeTtlISO8601 :: Int -> Text
computeTtlISO8601 ttlInSec =
  let ttlToNominalDiffTime = intToNominalDiffTime ttlInSec
   in Utils.formatTimeDifference ttlToNominalDiffTime

mapVariantToVehicle :: DTV.VehicleVariant -> Enums.VehicleCategory
mapVariantToVehicle = \case
  DTV.SEDAN -> Enums.CAB
  DTV.HATCHBACK -> Enums.CAB
  DTV.TAXI -> Enums.CAB
  DTV.SUV -> Enums.CAB
  DTV.TAXI_PLUS -> Enums.CAB
  DTV.PREMIUM_SEDAN -> Enums.CAB
  DTV.BLACK -> Enums.CAB
  DTV.BLACK_XL -> Enums.CAB
  DTV.BIKE -> Enums.MOTORCYCLE
  DTV.AUTO_RICKSHAW -> Enums.AUTO_RICKSHAW
  DTV.AMBULANCE_TAXI -> Enums.AMBULANCE
  DTV.AMBULANCE_TAXI_OXY -> Enums.AMBULANCE
  DTV.AMBULANCE_AC -> Enums.AMBULANCE
  DTV.AMBULANCE_AC_OXY -> Enums.AMBULANCE
  DTV.AMBULANCE_VENTILATOR -> Enums.AMBULANCE
  DTV.SUV_PLUS -> Enums.CAB
  DTV.HERITAGE_CAB -> Enums.CAB
  DTV.EV_AUTO_RICKSHAW -> Enums.AUTO_RICKSHAW
  DTV.DELIVERY_BIKE -> Enums.MOTORCYCLE
  DTV.DELIVERY_LIGHT_GOODS_VEHICLE -> Enums.TRUCK
  DTV.DELIVERY_TRUCK_MINI -> Enums.TRUCK
  DTV.DELIVERY_TRUCK_SMALL -> Enums.TRUCK
  DTV.DELIVERY_TRUCK_MEDIUM -> Enums.TRUCK
  DTV.DELIVERY_TRUCK_LARGE -> Enums.TRUCK
  DTV.DELIVERY_TRUCK_ULTRA_LARGE -> Enums.TRUCK
  DTV.BUS_NON_AC -> Enums.BUS
  DTV.BUS_AC -> Enums.BUS

castVehicleCategoryToDomain :: Enums.VehicleCategory -> DVC.VehicleCategory
castVehicleCategoryToDomain = \case
  Enums.CAB -> DVC.CAR
  Enums.AUTO_RICKSHAW -> DVC.AUTO_CATEGORY
  Enums.AMBULANCE -> DVC.AMBULANCE
  Enums.MOTORCYCLE -> DVC.MOTORCYCLE
  Enums.METRO -> DVC.TRAIN
  Enums.SUBWAY -> DVC.TRAIN
  _ -> DVC.CAR -- not used

mapServiceTierToCategory :: ServiceTierType -> Enums.VehicleCategory
mapServiceTierToCategory = \case
  SEDAN -> Enums.CAB
  HATCHBACK -> Enums.CAB
  TAXI -> Enums.CAB
  SUV -> Enums.CAB
  TAXI_PLUS -> Enums.CAB
  COMFY -> Enums.CAB
  ECO -> Enums.CAB
  PREMIUM -> Enums.CAB
  PREMIUM_SEDAN -> Enums.CAB
  BLACK -> Enums.CAB
  BLACK_XL -> Enums.CAB
  AUTO_RICKSHAW -> Enums.AUTO_RICKSHAW
  EV_AUTO_RICKSHAW -> Enums.AUTO_RICKSHAW
  BIKE -> Enums.MOTORCYCLE
  AMBULANCE_TAXI -> Enums.AMBULANCE
  AMBULANCE_TAXI_OXY -> Enums.AMBULANCE
  AMBULANCE_AC -> Enums.AMBULANCE
  AMBULANCE_AC_OXY -> Enums.AMBULANCE
  AMBULANCE_VENTILATOR -> Enums.AMBULANCE
  SUV_PLUS -> Enums.CAB
  HERITAGE_CAB -> Enums.CAB
  DELIVERY_BIKE -> Enums.MOTORCYCLE
  DELIVERY_LIGHT_GOODS_VEHICLE -> Enums.TRUCK
  DELIVERY_TRUCK_MINI -> Enums.TRUCK
  DELIVERY_TRUCK_SMALL -> Enums.TRUCK
  DELIVERY_TRUCK_MEDIUM -> Enums.TRUCK
  DELIVERY_TRUCK_LARGE -> Enums.TRUCK
  DELIVERY_TRUCK_ULTRA_LARGE -> Enums.TRUCK
  BUS_NON_AC -> Enums.BUS
  BUS_AC -> Enums.BUS

getListOfServiceTireTypes :: Enums.VehicleCategory -> [DVST.ServiceTierType]
getListOfServiceTireTypes Enums.CAB = [DVST.SEDAN, DVST.SUV, DVST.HATCHBACK, DVST.TAXI, DVST.TAXI_PLUS, DVST.ECO, DVST.COMFY, DVST.PREMIUM, DVST.PREMIUM_SEDAN, DVST.BLACK, DVST.BLACK_XL, DVST.SUV_PLUS, DVST.HERITAGE_CAB]
getListOfServiceTireTypes Enums.AUTO_RICKSHAW = [DVST.AUTO_RICKSHAW, DVST.EV_AUTO_RICKSHAW]
getListOfServiceTireTypes Enums.MOTORCYCLE = [DVST.BIKE, DVST.DELIVERY_BIKE]
getListOfServiceTireTypes Enums.TWO_WHEELER = [DVST.BIKE, DVST.DELIVERY_BIKE]
getListOfServiceTireTypes Enums.AMBULANCE = [DVST.AMBULANCE_TAXI, DVST.AMBULANCE_TAXI_OXY, DVST.AMBULANCE_AC, DVST.AMBULANCE_AC_OXY, DVST.AMBULANCE_VENTILATOR]
getListOfServiceTireTypes Enums.METRO = []
getListOfServiceTireTypes Enums.SUBWAY = []
getListOfServiceTireTypes Enums.BUS = [DVST.BUS_NON_AC, DVST.BUS_AC]
getListOfServiceTireTypes Enums.TRUCK = [DVST.DELIVERY_LIGHT_GOODS_VEHICLE, DVST.DELIVERY_TRUCK_MINI, DVST.DELIVERY_TRUCK_SMALL, DVST.DELIVERY_TRUCK_MEDIUM, DVST.DELIVERY_TRUCK_LARGE, DVST.DELIVERY_TRUCK_ULTRA_LARGE]

tripCategoryToFulfillmentType :: TripCategory -> Text
tripCategoryToFulfillmentType = \case
  -- Off-us Beckn-fulfillmentType Enums
  OneWay OneWayOnDemandDynamicOffer -> show Enums.DELIVERY
  d@(Delivery _) -> show d
  -- TODO :: To be removed after released ---- STARTS HERE
  OneWay OneWayRideOtp -> show Enums.RIDE_OTP
  CrossCity OneWayRideOtp _ -> show Enums.RIDE_OTP
  RideShare RideOtp -> show Enums.RIDE_OTP
  Rental _ -> show Enums.RENTAL
  InterCity _ _ -> show Enums.INTER_CITY
  Ambulance _ -> show Enums.AMBULANCE_FLOW
  _ -> show Enums.DELIVERY

-- TODO :: To be removed after released ---- ENDS HERE
-- On-us Domain-tripCategory -- TODO :: TO BE UNCOMMENTED AFTER RELEASE
-- tripCategory -> show tripCategory

fulfillmentTypeToTripCategory :: Text -> TripCategory
fulfillmentTypeToTripCategory fulfillmentType =
  case readMaybe @TripCategory $ T.unpack fulfillmentType of
    Just tripCategory -> tripCategory
    -- For off-us Beckn-fulfillmentType to Domain-tripCategory
    Nothing ->
      case readMaybe @Enums.FulfillmentType $ T.unpack fulfillmentType of
        Just Enums.DELIVERY -> OneWay OneWayOnDemandDynamicOffer
        Just Enums.RIDE_OTP -> OneWay OneWayRideOtp
        Just Enums.RENTAL -> Rental OnDemandStaticOffer
        Just Enums.INTER_CITY -> InterCity OneWayOnDemandStaticOffer Nothing
        Just Enums.AMBULANCE_FLOW -> Ambulance OneWayOnDemandDynamicOffer
        _ -> OneWay OneWayOnDemandDynamicOffer
