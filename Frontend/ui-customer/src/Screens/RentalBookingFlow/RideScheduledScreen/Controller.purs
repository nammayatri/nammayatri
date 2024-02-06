{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RideScheduledScreen.Controller where

import Accessor (_lat, _lon, _selectedQuotes, _fareProductType)
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SourceToDestination.Controller as SourceToDestinationActionController
import Components.SelectListModal.Controller as CancelRidePopUp
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Resources.Constants (cancelReasons)
import Data.Array ((!!), head)
import Log (trackAppActionClick, trackAppEndScreen)
import Prelude
import PrestoDOM (class Loggable, Eval, continue, exit)
import Screens (getScreen, ScreenName(..))
import Helpers.Utils (performHapticFeedback)
import Screens.Types (RideScheduledScreenState, City(..))
import Resources.Constants (cancelReasons, dummyCancelReason)
import JBridge (hideKeyboardOnNavigation)
import Services.API
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Data.Lens ((^.))
import Resources.Constants (getAddressFromBooking, decodeAddress, DecodeAddress(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    PrimaryButtonActionController _-> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "primary_button" "on_click"
    SourceToDestinationAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "source_to_destination" "dummy_action"
    CancelRide -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "cancel_ride" "on_click"
    AddFirstStop -> do 
      trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "add_first_stop" "on_click"
      trackAppEndScreen appId (getScreen RIDE_SCHEDULED_SCREEN)
    _ -> pure unit

data Action
  = NoAction
  | PrimaryButtonActionController PrimaryButtonController.Action
  | SourceToDestinationAC SourceToDestinationActionController.Action
  | CancelRide
  | AddFirstStop
  | GenericHeaderAC GenericHeaderController.Action
  | CancelRidePopUpAction CancelRidePopUp.Action
  | GetBookingList RideBookingListRes
  | CheckFlowStatusAction

data ScreenOutput = GoToHomeScreen 
                  | GoToSearchLocationScreen RideScheduledScreenState
                  | CancelRentalRide RideScheduledScreenState

eval :: Action -> RideScheduledScreenState -> Eval Action ScreenOutput RideScheduledScreenState

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = exit $ GoToHomeScreen

eval AddFirstStop state = exit $ GoToSearchLocationScreen state

eval (SourceToDestinationAC (SourceToDestinationActionController.DestinationClicked)) state = exit $ GoToSearchLocationScreen state

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = exit $ GoToHomeScreen

eval CancelRide state = continue state{ props{isCancelRide = true}, data{cancellationReasons = cancelReasons ""}}

eval (CancelRidePopUpAction (CancelRidePopUp.Button1 PrimaryButtonController.OnClick)) state = do 
  void $ pure $ performHapticFeedback unit 
  continue state{props{isCancelRide = false}}

eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  let newState = state{props{isCancelRide = false}}
  case state.props.cancelRideActiveIndex of
    Just index -> if ( (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRentalRide newState{props{cancelDescription = if (newState.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.data.cancellationReasons !!index)).description else newState.props.cancelDescription }}
                    else exit $ CancelRentalRide newState{props{cancelDescription = (fromMaybe dummyCancelReason (state.data.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode }}
    Nothing    -> continue state

eval (CancelRidePopUpAction (CancelRidePopUp.OnGoBack)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.UpdateIndex index)) state = continue state { props { cancelRideActiveIndex = Just index, cancelReasonCode = (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode } }

eval (CancelRidePopUpAction (CancelRidePopUp.TextChanged valId newVal)) state = continue state { props { cancelDescription = newVal } }

eval (CancelRidePopUpAction (CancelRidePopUp.ClearOptions)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  continue state { props { cancelDescription = "", cancelReasonCode = "", cancelRideActiveIndex = Nothing } }

eval (GetBookingList resp) state =
    let (RideBookingListRes listResp) = resp
        (RideBookingRes resp) = fromMaybe HomeScreenData.dummyRideBooking $ head listResp.list
        (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
        (RideBookingDetails contents) = bookingDetails.contents
    in continue state { data
      { source = SearchLocationScreenData.dummyLocationInfo { lat = Just (resp.fromLocation ^._lat) , lon = Just (resp.fromLocation ^._lon), placeId = Nothing, city = AnyCity, addressComponents = getAddressFromBooking resp.fromLocation, address = decodeAddress (Booking resp.fromLocation)}
      , destination = maybe (Nothing) (\toLocation -> Just $ SearchLocationScreenData.dummyLocationInfo {lat = Just (toLocation^._lat), lon = Just (toLocation^._lon), placeId = Nothing, city = AnyCity, addressComponents = getAddressFromBooking toLocation, address = decodeAddress (Booking toLocation)}) contents.stopLocation
      , startTime = fromMaybe "" resp.rideScheduledTime
      , finalPrice = show resp.estimatedTotalFare
      , baseDuration = show $ (fromMaybe 7200 resp.estimatedDuration)/3600
      , baseDistance = show $ (fromMaybe 20000 resp.estimatedDistance)/1000
      , bookingId = resp.id}
      , props{driverAllocationTime = "15" } -- TODO-codex : Need to get the driver allocation time from the API 
      }

eval _ state = continue state