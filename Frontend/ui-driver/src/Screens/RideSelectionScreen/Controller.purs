{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Controller where

import Log
import Data.Array (length, union, filter, (!!))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust,maybe)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (strToBool, convertUTCtoISC)
import Helpers.Utils (parseFloat, setEnabled, setRefreshing)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (class Show, bind, discard, map, not, pure, unit, ($), (&&), (+), (/), (/=), (<>), (==), (||))
import PrestoDOM (Eval, update, ScrollState(..), continue, exit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress, rideTypeConstructor)
import Screens (ScreenName(..), getScreen)
import Screens.Types (AnimationState(..), IndividualRideCardState, ItemState, RideSelectionScreenState)
import Services.API (RidesInfo(..))
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Styles.Colors as Color
import Components.ErrorModal as ErrorModalController
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils as HU

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    Refresh            -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "refresh"
    Scroll str         -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "scroll"
    AfterRender        -> trackAppScreenRender appId "screen" (getScreen RIDE_SELECTION_SCREEN)
    OnFadeComplete str -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "on_fade_complete"
    Loader             -> do
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "loader"
                          trackAppEndScreen   appId (getScreen RIDE_SELECTION_SCREEN)
    BackPressed        -> do
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "back_pressed"
                          trackAppEndScreen   appId (getScreen RIDE_SELECTION_SCREEN)
    DontKnowRide (PrimaryButton.OnClick) ->
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "dont_know_ride"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "bottom_nav_bar" "on_navigate"
                          trackAppEndScreen   appId (getScreen RIDE_SELECTION_SCREEN)
    IndividualRideCardAction (IndividualRideCardController.Select index) -> do
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "individual_ride_card_action" "select"
                          trackAppEndScreen   appId (getScreen RIDE_SELECTION_SCREEN)
    ScrollStateChanged scrollState ->
                          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "scroll_state_changed"
    _                  -> pure unit

data ScreenOutput = GoBack
                  | SelectRide    RideSelectionScreenState
                  | LoaderOutput  RideSelectionScreenState
                  | RefreshScreen RideSelectionScreenState

data Action = Loader
            | Refresh
            | NoAction
            | AfterRender
            | BackPressed
            | Scroll             String
            | DontKnowRide       PrimaryButton.Action
            | OnFadeComplete     String
            | ScrollStateChanged ScrollState
            | BottomNavBarAction BottomNavBar.Action
            | IndividualRideCardAction     IndividualRideCardController.Action
            | ErrorModalActionController   ErrorModalController.Action
            | RideHistoryAPIResponseAction (Array RidesInfo)

eval :: Action -> RideSelectionScreenState -> Eval Action ScreenOutput RideSelectionScreenState

eval AfterRender state =
  continue state

eval BackPressed state =
  exit GoBack

eval (OnFadeComplete _ ) state =
  if (not state.recievedResponse)
  then
    continue state
  else
    continue state {
      shimmerLoader = case state.shimmerLoader of
                        AnimatedIn -> AnimatedOut
                        AnimatingOut -> AnimatedOut
                        a -> a
    }
                                      
eval Refresh state = do
  exit $ RefreshScreen state

eval (DontKnowRide (PrimaryButton.OnClick)) state = do
  exit $ SelectRide state {
      selectedItem = Nothing
  }

eval (ScrollStateChanged scrollState) state = do
  _ <- case scrollState of
           SCROLL_STATE_FLING ->
               pure $ setEnabled "2000030" false
           _ ->
               pure unit
  continue state

eval (IndividualRideCardAction (IndividualRideCardController.Select index)) state = do
  exit $ SelectRide state {
      selectedItem = state.rideList !! index
  }

eval Loader state = do
  exit $ LoaderOutput state

eval (RideHistoryAPIResponseAction rideList) state = do
  let bufferCardDataPrestoList = (rideHistoryListTransformer rideList $ fromMaybe "" state.selectedCategory.categoryAction)
  let filteredRideList         = (rideListResponseTransformer rideList $ fromMaybe "" state.selectedCategory.categoryAction)
  _ <- pure $ setRefreshing "2000030" false
  let loadBtnDisabled          = length rideList == 0
  continue state {
    shimmerLoader = AnimatedOut
  , recievedResponse = true
  , rideList = union (state.rideList) (filteredRideList)
  , prestoListArrayItems =  union (state.prestoListArrayItems) (bufferCardDataPrestoList)
  , loadMoreDisabled = loadBtnDisabled
  }

eval (Scroll value) state = do
  let firstIndex     = fromMaybe 0    (fromString (fromMaybe "0"    ((split (Pattern ",")(value))!!0)))
  let visibleItems   = fromMaybe 0    (fromString (fromMaybe "0"    ((split (Pattern ",")(value))!!1)))
  let totalItems     = fromMaybe 0    (fromString (fromMaybe "0"    ((split (Pattern ",")(value))!!2)))
  let canScrollUp    = fromMaybe true (strToBool  (fromMaybe "true" ((split (Pattern ",")(value))!!3)))
  let loadMoreButton = totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems
  _ <- if canScrollUp
       then (pure $ setEnabled "2000030" false)
       else (pure $ setEnabled "2000030" true)
  continue state { loaderButtonVisibility = loadMoreButton}

eval _ state =
  continue state

rideHistoryListTransformer :: Array RidesInfo -> String -> Array ItemState
rideHistoryListTransformer list categoryAction =
  ( map (\ (RidesInfo ride) ->
    let specialLocationConfig = HU.getRideLabelData ride.specialLocationTag
    in
    { id     : toPropValue ride.id
    , date   : toPropValue (convertUTCtoISC (ride.createdAt) "D MMM")
    , time   : toPropValue (convertUTCtoISC (ride.createdAt) "h:mm A")
    , status : toPropValue (ride.status)
    , source : toPropValue (decodeAddress (ride.fromLocation) false)
    , updatedAt  : toPropValue ride.updatedAt
    , driverName : toPropValue ride.driverName
    , destination  : toPropValue (maybe "" (\toLocation -> decodeAddress (toLocation) false) ride.toLocation)
    , shortRideId  : toPropValue ((getString TRIP_ID )<> ": " <> ride.shortRideId)
    , rideDistance : toPropValue $ (parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2) <> " km " <> (getString RIDE) <> case ride.riderName of
                                      Just name -> " " <> (getString WITH) <> " " <> name
                                      Nothing -> ""
    , vehicleColor : toPropValue ride.vehicleColor
    , vehicleModel : toPropValue ride.vehicleModel
    , amountColor  : toPropValue (case (ride.status) of
                                    "COMPLETED" -> Color.black800
                                    "CANCELLED" -> Color.red
                                    _ -> Color.black800
                                 )
    , total_amount : toPropValue (case (ride.status) of
                                    "CANCELLED" -> 0
                                    _           -> fromMaybe ride.estimatedBaseFare ride.computedFare
                                 )
    , vehicleNumber      : toPropValue ride.vehicleNumber
    , card_visibility    : toPropValue "visible"
    , shimmer_visibility : toPropValue "gone"
    , driverSelectedFare : toPropValue ride.driverSelectedFare
    , riderName : toPropValue $ fromMaybe "" ride.riderName
    , spLocTagVisibility : toPropValue if isJust ride.specialLocationTag && isJust (HU.getRequiredTag ride.specialLocationTag) then "visible" else "gone"
    , specialZoneText : toPropValue $ specialLocationConfig.text
    , specialZoneImage : toPropValue $ specialLocationConfig.imageUrl
    , specialZoneLayoutBackground : toPropValue $ specialLocationConfig.backgroundColor
    , gotoTagVisibility : toPropValue if isJust ride.driverGoHomeRequestId then "visible" else "gone"
    , purpleTagVisibility : toPropValue if isJust ride.disabilityTag then "visible" else "gone"
    , tipTagVisibility : toPropValue if isJust ride.customerExtraFee then "visible" else "gone"
    , specialZonePickup : toPropValue if (HU.checkSpecialPickupZone ride.specialLocationTag) then "visible" else "gone"
    }
  ) (filter (\(RidesInfo ride) -> ((ride.status /= "CANCELLED" && categoryAction == "LOST_AND_FOUND") || (categoryAction /= "LOST_AND_FOUND"))) list))


rideListResponseTransformer :: Array RidesInfo -> String -> Array IndividualRideCardState
rideListResponseTransformer list categoryAction =
  (map (\ (RidesInfo ride) ->
    { id   : ride.id
    , date : (convertUTCtoISC (ride.createdAt) "D MMM")
    , time : (convertUTCtoISC (ride.createdAt )"h:mm A")
    , source : (decodeAddress (ride.fromLocation) false)
    , status :  (ride.status)
    , updatedAt   : ride.updatedAt
    , driverName  : ride.driverName
    , destination : maybe "" (\toLocation -> decodeAddress (toLocation) false) ride.toLocation
    , shortRideId : ride.shortRideId
    , vehicleColor : ride.vehicleColor
    , rideDistance : parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2
    , vehicleModel : ride.vehicleModel
    , total_amount : (case (ride.status) of
                        "CANCELLED" -> 0
                        _ -> fromMaybe ride.estimatedBaseFare ride.computedFare
                     )
    , vehicleNumber   :  ride.vehicleNumber
    , card_visibility : (case (ride.status) of
                           "CANCELLED" -> "gone"
                           _ -> "visible"
                        )
    , shimmer_visibility : "gone"
    , driverSelectedFare : ride.driverSelectedFare
    , vehicleType : ride.vehicleVariant
    , riderName : fromMaybe "" ride.riderName
    , customerExtraFee : Nothing
    , purpleTagVisibility : false
    , gotoTagVisibility : false
    , spLocTagVisibility : false 
    , specialZoneLayoutBackground : ""
    , specialZoneImage : ""
    , specialZoneText : ""
    , specialZonePickup : false
    , tripType : rideTypeConstructor ride.tripCategory
    , tollCharge : fromMaybe 0.0 ride.tollCharges
    , rideType : ride.vehicleServiceTierName
    , tripStartTime : ride.tripStartTime
    , tripEndTime : ride.tripEndTime
    , acRide : ride.isVehicleAirConditioned
    , vehicleServiceTier : ride.vehicleServiceTier
    , parkingCharge : fromMaybe 0.0 ride.parkingCharge
    , stops : fromMaybe [] ride.stops
    }
  ) (filter (\(RidesInfo ride) -> ((ride.status /= "CANCELLED" && categoryAction == "LOST_AND_FOUND") || (categoryAction /= "LOST_AND_FOUND"))) list))

