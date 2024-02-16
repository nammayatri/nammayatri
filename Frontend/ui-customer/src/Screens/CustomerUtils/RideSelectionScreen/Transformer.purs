{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Transformer where

import Accessor (_computedPrice, _contents, _driverName, _estimatedDistance, _id, _otpCode, _rideRating, _toLocation, _vehicleNumber)
import Common.Types.App (LazyCheck(..))
import Data.Array (filter, null, (!!))
import Data.Lens ((^.))
import Data.Maybe (fromMaybe, isJust)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Helpers.Utils (FetchImageFrom(..), fetchImage, isHaveFare, withinTimeRange)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (map, show, ($), (&&), (+), (-), (/=), (<>), (==), (||))
import PrestoDOM.Types.Core (toPropValue)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getFareFromArray, getKmMeter, fetchVehicleVariant)
import Resources.Localizable.EN (getEN)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getSpecialTag)
import Screens.Types (Fares, IndividualRideCardState, ItemState, RideSelectionScreenState, Stage(..), ZoneType(..))
import Services.API (FareBreakupAPIEntity, RideAPIEntity(..), RideBookingRes(..))
import Storage (isLocalStageOn)
import Data.Ord (abs)
import ConfigProvider


myRideListTransformerProp :: Array RideBookingRes  -> Array ItemState
myRideListTransformerProp listRes =  filter (\item -> (item.status == (toPropValue "COMPLETED") || item.status == (toPropValue "CANCELLED"))) (map (\(RideBookingRes ride) -> 

  let
    rideApiEntity = fromMaybe dummyRideAPIEntity (ride.rideList !!0)
  in {
    date : toPropValue (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
    time : toPropValue (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
    source : toPropValue $ decodeAddress $ Booking ride.fromLocation,
    destination : toPropValue $ decodeAddress $ Booking $ ride.bookingDetails ^._contents^._toLocation,
    totalAmount : toPropValue $ (getCurrency appConfig) <> " " <> (show $ fromMaybe 0 $ (fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice),
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "gone",
    driverImage : toPropValue $ fetchImage FF_ASSET "ny_ic_user",
    isCancelled : toPropValue if ride.status == "CANCELLED" then "visible" else "gone",
    isSuccessfull : toPropValue if ride.status == "COMPLETED" then "visible" else "gone",
    rating : toPropValue $ fromMaybe 0 $ rideApiEntity^. _rideRating,
    driverName : toPropValue $ rideApiEntity^. _driverName,
    rideStartTime : toPropValue $ convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A",
    rideEndTime : toPropValue $ convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A",
    vehicleNumber : toPropValue $ rideApiEntity^._vehicleNumber,
    rideId : toPropValue $ rideApiEntity^._id,
    status : toPropValue ride.status,
    rideEndTimeUTC : toPropValue $ fromMaybe ride.createdAt ride.rideEndTime,
    alpha : toPropValue if isLocalStageOn HomeScreen then "1.0" else "0.5",
    zoneVisibility : toPropValue if (getSpecialTag ride.specialLocationTag).priorityTag == METRO then "visible" else "gone"
  })
  
   listRes)


myRideListTransformer :: RideSelectionScreenState -> Array RideBookingRes -> Array IndividualRideCardState
myRideListTransformer state listRes = filter (\item -> (item.status == "COMPLETED" || item.status == "CANCELLED")) (map (\(RideBookingRes ride) ->
  let
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    specialTags = getSpecialTag ride.specialLocationTag
    referenceString' = (if nightChargesVal && (getMerchant FunctionCall) /= YATRI then "1.5" <> getEN DAYTIME_CHARGES_APPLICABLE_AT_NIGHT else "")
                        <> (if isHaveFare "DRIVER_SELECTED_FARE" updatedFareList then "\n\n" <> getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO else "")
                        <> (if isHaveFare "WAITING_CHARGES" updatedFareList then "\n\n" <> getEN WAITING_CHARGE_DESCRIPTION else "")
                        <> (if isHaveFare "EARLY_END_RIDE_PENALTY" updatedFareList then "\n\n" <> getEN EARLY_END_RIDE_CHARGES_DESCRIPTION else "")
                        <> (if isHaveFare "CUSTOMER_SELECTED_FARE" updatedFareList then "\n\n" <> getEN CUSTOMER_TIP_DESCRIPTION else "")
  in {
    date : (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") ,
    time :  convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A",
    source :  decodeAddress $ Booking ride.fromLocation,
    destination : decodeAddress $ Booking $ ride.bookingDetails ^._contents^._toLocation,
    totalAmount :  ((getCurrency appConfig)) <> " " <> (show (fromMaybe 0 rideDetails.computedPrice)),
    cardVisibility :  "visible",
    shimmerVisibility :  "gone",
    driverImage :  fetchImage FF_ASSET "ny_ic_user",
    isCancelled :  if ride.status == "CANCELLED" then "visible" else "gone",
    isSuccessfull :  if ride.status == "COMPLETED" then "visible" else "gone",
    rating : fromMaybe 0 rideDetails.rideRating,
    driverName : rideDetails.driverName,
    rideStartTime : convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A",
    rideEndTime : convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A",
    vehicleNumber : rideDetails.vehicleNumber,
    rideId : rideDetails.id,
    status : ride.status,
    shortRideId : rideDetails.shortRideId,
    bookingId : ride.id,
    rideEndTimeUTC : fromMaybe "" ride.rideEndTime,
    sourceLocation : ride.fromLocation,
    destinationLocation : ((ride.bookingDetails)^._contents)^._toLocation,
    alpha : if isLocalStageOn HomeScreen then "1.0" else "0.5"
  , fareBreakUpList : fares
  , faresList : updatedFareList
  , baseFare : fares.baseFare
  , pickupCharges : fares.pickupCharges
  , extraFare : (getCurrency appConfig) <> " " <> (show $ getFareFromArray ride.fareBreakup "EXTRA_DISTANCE_FARE")
  , waitingCharges : fares.waitingCharges
  , baseDistance : baseDistanceVal
  , extraDistance : getKmMeter $  abs $ (fromMaybe 0 rideDetails.chargeableRideDistance) - (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance))
  , referenceString : referenceString'
  , nightCharges : nightChargesVal
  , isSpecialZone : null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode)
  , zoneType : specialTags.priorityTag
  , optionsVisibility : false
  , isSrcServiceable: state.data.isSrcServiceable
  , vehicleVariant : fetchVehicleVariant rideDetails.vehicleVariant
  , merchantExoPhone : ride.merchantExoPhone
  }) listRes)

matchRidebyId :: IndividualRideCardState -> IndividualRideCardState -> Boolean
matchRidebyId rideOne rideTwo = rideOne.bookingId == rideTwo.bookingId

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare : (getCurrency appConfig) <>  " " <> (show $ ((getFareFromArray fares "BASE_FARE") + (getFareFromArray fares "EXTRA_DISTANCE_FARE")) - 10)
, pickupCharges : (getCurrency appConfig) <> " 10.0"
, waitingCharges : (getCurrency appConfig) <> " " <> (show $ getFareFromArray fares "WAITING_CHARGES")
, nominalFare : (getCurrency appConfig) <> " " <> (show $ getFareFromArray fares "DRIVER_SELECTED_FARE")
}