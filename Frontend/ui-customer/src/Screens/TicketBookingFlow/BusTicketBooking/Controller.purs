{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.BusTicketBooking.Controller where

import Common.Types.App as App
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PopUpModal.Controller as PopUpModalController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Screens.TicketBookingFlow.MetroMyTickets.ScreenData as MetroMyTicketsScreenData
import Screens.TicketBookingFlow.BusTicketBooking.ScreenData as BusTicketBookingScreenData
import Screens.TicketBookingFlow.MetroMyTickets.Transformer (metroTicketListApiToMyTicketsTransformer)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Constants.Configs (getPolylineAnimationConfig)
import Data.Array as DA
import Data.Maybe( Maybe(..), fromMaybe, maybe, isNothing)
import Data.Number (fromString, round) as NUM
import Data.Tuple
import Debug (spy)
import Effect.Aff (launchAff)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Prelude
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Services.API as API
import Screens.Types as ST
import Services.Backend as Remote
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalStore)
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Effect (Effect)
import Effect.Uncurried (runEffectFn4)
import Data.Lens ((^.))
import Accessor (_lat, _lon, _routeCode)
import Data.Foldable (for_)
import JBridge (firebaseLogEvent)
import MapUtils as MU
import Data.String as DS
import Engineering.Helpers.Events as Events
import Effect.Unsafe (unsafePerformEffect)


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action 
  = NoAction
  | EditInfo Boolean
  | GoBack
  | GenericHeaderAC GenericHeaderController.Action
  | SearchButtonClick
  | TicketIconClicked
  | SourceToDestinationAC SourceToDestinationController.Action
  | BusTicketBookingListRespAC (Array API.FRFSTicketBookingStatusAPIRes)
  | TicketPressed API.FRFSTicketBookingStatusAPIRes
  | RepeatRideClicked API.FRFSTicketBookingStatusAPIRes
  | ViewMoreClicked
  | MapReady String String String
  | UpdateCurrentLocation String String
  | RecenterCurrentLocation
  | NearbyDriverRespAC API.NearbyDriverRes
  | UpdateClosestBusZoomLevel Number

data ScreenOutput
  = GoToHomeScreen ST.BusTicketBookingState
  | RefreshScreen ST.BusTicketBookingState
  | GoToMyTicketsScreen ST.BusTicketBookingState
  | GoToChooseYourRide ST.BusTicketBookingState
  | GoToConfirmgDelivery ST.BusTicketBookingState
  | GoToSearchLocationScreenForRoutes ST.BusTicketBookingState ST.LocationActionId
  | GoToMetroTicketDetailsFlow String
  | GoToMetroTicketDetailsScreen API.FRFSTicketBookingStatusAPIRes

updateCurrentLocation :: ST.BusTicketBookingState -> String -> String -> Eval Action  ScreenOutput ST.BusTicketBookingState
updateCurrentLocation state lat lng = 
  let updatedState = state { props { srcLat = fromMaybe 0.0 (NUM.fromString lat), srcLong = fromMaybe 0.0 (NUM.fromString lng) } }
  in continue updatedState
      
eval :: Action -> ST.BusTicketBookingState -> Eval Action ScreenOutput ST.BusTicketBookingState

eval GoBack state = exit $ GoToHomeScreen state

eval SearchButtonClick state = do
  let _ = unsafePerformEffect $ Events.addEventAggregate "ny_bus_user_clicked_search_Location_bus"
  updateAndExit state $ GoToSearchLocationScreenForRoutes state ST.Src

eval (BusTicketBookingListRespAC bookingList) state =
  let newState = state {data {ticketDetailsState = Just $ metroTicketListApiToMyTicketsTransformer bookingList $ fromMaybe MetroMyTicketsScreenData.initData state.data.ticketDetailsState}}
      -- newState = state {data {ticketDetailsState = Just $ metroTicketListApiToMyTicketsTransformer bookingList $ fromMaybe MetroMyTicketsScreenData.initData state.data.ticketDetailsState}}
      allTicketsList = DA.concatMap (\(API.FRFSTicketBookingStatusAPIRes list) -> list.tickets) bookingList
      isActiveTicket = DA.find (\(API.FRFSTicketAPI ticket) -> ticket.status == "ACTIVE") allTicketsList
  -- in continue newState
  in if isNothing isActiveTicket then do
      void $ pure $ deleteValueFromLocalStore ONBOARDED_VEHICLE_INFO
      -- void $ pure $ deleteValueFromLocalStore CAN_HAVE_ACTIVE_TICKETS
      continue newState
    else continue newState
    -- continue newState
    -- case (DA.find (\(API.FRFSTicketBookingStatusAPIRes list) -> list.status == "ACTIVE") bookingList) of
    -- Just _ -> continue newState
    -- Nothing -> do
    --   void $ pure $ deleteValueFromLocalStore ONBOARDED_VEHICLE_INFO
    --   void $ pure $ deleteValueFromLocalStore CAN_HAVE_ACTIVE_TICKETS
    -- continue newState

eval TicketIconClicked state = updateAndExit state $ GoToMyTicketsScreen state

eval (TicketPressed (API.FRFSTicketBookingStatusAPIRes ticketApiResp)) state = do 
  updateAndExit state $ GoToMetroTicketDetailsFlow ticketApiResp.bookingId

eval (RepeatRideClicked ticketApiResp) state = 
  updateAndExit state $ GoToMetroTicketDetailsScreen ticketApiResp

eval (ViewMoreClicked) state =
  continue state { props { showAllTickets = not state.props.showAllTickets }}


eval (MapReady _ _ _) state = 
  continueWithCmd state { props { gotMapReady = true } } [ do
    -- let markerName = HU.getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
    --     markerConfig = JB.defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
    -- _ <- pure $ JB.currentPosition ""
    void $ pure $ JB.removeAllMarkers ""
   -- void $ runEffectFn4 JB.showDynamicRouteMarker (show state.props.srcLat) (show state.props.srcLong) "AC23A-U" (EHC.getNewIDWithTag "BusTicketBookingScreenMap")
    pure NoAction
  ]

eval (UpdateCurrentLocation lat lng) state = updateCurrentLocation state lat lng

eval RecenterCurrentLocation state = do
  recenterCurrentLocation state

eval (NearbyDriverRespAC (API.NearbyDriverRes resp)) state =
  let newState = state{data{busDetailsArray = DA.concatMap transformNearByDriversBucketResp resp.buckets}}
  in continueWithCmd newState [do
    showAllMarkersOnMap newState.data.busDetailsArray
    pure NoAction
  ]
  where
    showAllMarkersOnMap busDetailsArray = do
      for_ busDetailsArray \busDetails -> do
        case busDetails.routeCode of
          Just routeCode -> do
            let routeCodeFinal = fromMaybe "" $ DA.head $ DS.split (DS.Pattern "-") routeCode
            when (state.props.gotMapReady) $ void $ runEffectFn4 JB.showDynamicRouteMarker (show busDetails.lat) (show busDetails.lon) routeCodeFinal (EHC.getNewIDWithTag "BusTicketBookingScreenMap")
          Nothing -> pure unit

    transformNearByDriversBucketResp (API.NearByDriversBucket nearByDriversBucket) = 
      map filterOutLatLongFromDriverInfo nearByDriversBucket.driverInfo

    filterOutLatLongFromDriverInfo (API.DriverInfo driverInfo) =
      { lat: driverInfo.lat, lon: driverInfo.lon, routeCode: filterBusOnlyRouteCode driverInfo.rideDetails}

    filterBusOnlyRouteCode mbRideDetails = 
      case mbRideDetails of
        Just (API.RideDetails rideDetails) -> 
          case rideDetails.rideInfo of
            Just (API.Bus (API.BusRideInfo busRideInfo)) -> 
              let (API.BusContentType busContents) = busRideInfo.contents 
              in Just $ busContents.routeCode
            _ -> Nothing
        _ -> Nothing

eval (UpdateClosestBusZoomLevel closestDistance) state =
  continue state { data { closestBusDistance = closestDistance } }

eval _ state = continue state


showMarkerOnMap :: String -> Number -> Number -> Effect Unit
showMarkerOnMap markerName lat lng = do
  let markerConfig = JB.defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName, zIndex = 0.0}
  void $ JB.showMarker markerConfig lat lng 160 0.5 0.9 (EHC.getNewIDWithTag "BusTicketBookingScreenMap")

recenterCurrentLocation :: ST.BusTicketBookingState -> Eval Action ScreenOutput ST.BusTicketBookingState
recenterCurrentLocation state = continueWithCmd state [ do
    if state.props.locateOnMap then do
      _ <- pure $ JB.currentPosition "NO_ZOOM"
      pure unit
    else do
      let markerName = HU.getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
          markerConfig = JB.defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
      _ <- pure $ JB.currentPosition ""
      void $ showMarkerOnMap (HU.getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME) 9.9 9.9
      pure unit
    -- let newState = state{data{source = state.props.currentLocation.place}}
    pure NoAction
  ]
