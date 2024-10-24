{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.Controller where

import Engineering.Helpers.Commons as EHC
import Data.Traversable (traverse, for_)
import JBridge as JB
import Data.Time.Duration (Milliseconds(..))
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import Debug
import Prelude 
import PrestoDOM 
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData
import Helpers.Utils as HU
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM.Core (processEvent)
import Screens.Types as ST
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.BannerCarousel as BannerCarousel
import RemoteConfig as RC
import Data.String as DS
import Effect.Unsafe (unsafePerformEffect)
import Locale.Utils (getLanguageLocale)
import Constants (languageKey)
import SessionCache (getValueFromWindow)
import Data.Array as DA
import Data.Int as DI
import Data.Maybe as Mb
import PrestoDOM.List (ListItem)
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader.Controller as DropDownWithHeader
import Components.PrimaryButton as PrimaryButton
import Services.API as API
import Helpers.API as HelpersAPI
-- import Screens.MultiModalFlow.Components.MetroCard as MetroCard
-- import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
import Effect.Aff (launchAff)
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (GlobalState(..), defaultGlobalState)
import Common.Types.App as CTA
import Data.Lens((^.))
import Accessor (_lat, _lon)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Constants.Configs (getPolylineAnimationConfig)
import Screens.TicketBookingFlow.BusTrackingScreen.Transformer
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Either 

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data ScreenOutput
  = Exit ST.BusTrackingScreenState
  | GoToBusTicketBooking ST.BusTrackingScreenState
  | GoToSearchLocation ST.BusTrackingScreenState
  | GoToViewTicket ST.BusTrackingScreenState

data Action
  = AfterRender
  | BackPressed
  -- | MetroCardAction MetroCard.Action
  -- | VehicleCardAction VehicleCard.Action
  | MapReady String String String
  | NoAction
  | BookTicketButtonAction PrimaryButton.Action
  | ToggleStops
  | UpdateStops API.GetMetroStationResponse
  | ViewTicket
  -- | API.BusTrackingRouteResp

eval :: Action -> ST.BusTrackingScreenState -> Eval Action ScreenOutput ST.BusTrackingScreenState

-- eval (MetroCardAction MetroCard.ExpandStops) state = continue state

-- eval (VehicleCardAction VehicleCard.ExpandStops) state = continue state

eval (MapReady _ _ _ )state = continueWithCmd state [do 
  -- void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
    -- drawDriverRouteMock mockRoute
  --   -- defaultMockInviteFlow 0 state 
  --   let _ = spy "defaultMockInviteFlow" "defaultMockInviteFlow"
  --   pure unit
  pure NoAction
]

eval ToggleStops state = do
  let ht = JB.getLayoutBounds $ EHC.getNewIDWithTag $ "aaaaaaa" <> "false"
  continue state { props { expandStopsView = not state.props.expandStopsView, verticalLineHeight = ht.height }}

eval (UpdateStops (API.GetMetroStationResponse metroResponse)) state =  
  continueWithCmd state{data{stopsList = metroResponse}} [do 
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
      drawDriverRoute mockRoute metroResponse state 
      -- drawDriverRouteMock mockRoute
      let _ = spy "defaultMockInviteFlow" "defaultMockInviteFlow"
      pure unit
    pure NoAction
  ]

eval (BookTicketButtonAction PrimaryButton.OnClick) state = exit $ GoToSearchLocation state

eval BackPressed state = exit $ GoToBusTicketBooking state
eval ViewTicket state = exit $ GoToViewTicket state

eval _ state = update state


drawDriverRoute ::  API.Route -> Array API.GetMetroStationResp -> ST.BusTrackingScreenState -> Flow GlobalState Unit
drawDriverRoute route resp state = do
  let srcCode = Mb.maybe "" (\item -> item.stationCode) state.data.sourceStation
      destinationCode = Mb.maybe "" (\item -> item.stationCode) state.data.destinationStation
  response <- HelpersAPI.callApi $ API.FrfsGetRouteReq state.data.busRouteCode (getValueToLocalStore CUSTOMER_LOCATION) "BUS"
  let route = case response of 
                Right (API.FrfsGetRouteResp routeResp) ->
                  API.Route
                    { boundingBox: Mb.Nothing
                    , distance: 1671
                    , duration: 150
                    , pointsForRentals: Mb.Nothing
                    , points:
                        API.Snapped 
                          $ Mb.fromMaybe [] routeResp.waypoints 
                    , snappedWaypoints : API.Snapped []
                    }
                Left err ->  API.Route
                  { boundingBox: Mb.Nothing
                  , distance: 1671
                  , duration: 150
                  , pointsForRentals: Mb.Nothing
                  , points:
                      API.Snapped 
                        [ ]
                    , snappedWaypoints : API.Snapped []
                  }
  let routeConfig = spy  "transformStationsForMap" $ transformStationsForMap resp route srcCode destinationCode
  void $ delay $ Milliseconds 1000.0 
  void $ pure $ JB.removeAllPolylines ""
  void $ pure $ JB.removeAllMarkers ""
  EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  -- EHC.liftFlow $ JB.showMarker routeConfig.sourceMarkerConfig (EHC.getNewIDWithTag "BusTrackingScreenMap")
  -- void $ runExceptT $ runBackT $ Remote.drawMapRoute srcPoint.lat srcPoint.lng ride.destinationLat ride.destinationLng srcMarkerConfig destMarkerConfig "NORMAL" route "trip" $ (HSConfig.specialLocationConfig "" "" false getPolylineAnimationConfig) { autoZoom = false }
  -- when showRipples $ do
  --   EHC.liftFlow $ addAndUpdateSOSRipples srcPoint
  for_ (routeConfig.stopMarkerConfigs) $ \(item) -> do
      -- void $ map (\(API.VehicleInfo item) -> do
        EHC.liftFlow $ JB.showMarker item item.position.lat item.position.lng 40 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  void $ EHC.liftFlow $ JB.showMarker routeConfig.startMarkerConfig routeConfig.startMarkerConfig.position.lat routeConfig.startMarkerConfig.position.lng 130 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  void $ EHC.liftFlow $ JB.showMarker routeConfig.endMarkerConfig routeConfig.endMarkerConfig.position.lat routeConfig.endMarkerConfig.position.lng 130 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  EHC.liftFlow $ JB.animateCamera routeConfig.startMarkerConfig.position.lat routeConfig.startMarkerConfig.position.lng 10.0 "NO_ZOOM"


-- defaultMockInviteFlow :: Int -> ST.BusTrackingScreenState -> Array API.GetMetroStationResp  -> Flow GlobalState Unit
-- defaultMockInviteFlow id state resp  = do
--   -- localDelay 1000.0
--   let
--     srcPoint = getPoint mockDriverLocation
--     _ = spy "srcPoint" srcPoint
--   -- pure unit
--   -- pushAction $ UpdateMockData mockDriverInfo { vehicleDetails = "AUTO_RICKSHAW", vehicleVariant = "AUTO_RICKSHAW"}
--   drawDriverRoute mockDriverInfo srcPoint mockRoute false

drawDriverRouteMock ::  API.Route -> Flow GlobalState Unit
drawDriverRouteMock  (API.Route route)  = do
  let markers = HU.normalRoute ""
      srcPoint = getPoint mockDriverLocation
      srcMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, primaryText = "getString SOS_LOCATION" }
      
      destMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = "getString DROP" }
  let routeConfig = spy "mkRouteConfig" $ JB.mkRouteConfig (Remote.walkCoordinates route.points) srcMarkerConfig destMarkerConfig Mb.Nothing "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig
  EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  -- void $ runExceptT $ runBackT $ Remote.drawMapRoute srcPoint.lat srcPoint.lng ride.destinationLat ride.destinationLng srcMarkerConfig destMarkerConfig "NORMAL" route "trip" $ (HSConfig.specialLocationConfig "" "" false getPolylineAnimationConfig) { autoZoom = false }
  -- when showRipples $ do
  --   EHC.liftFlow $ addAndUpdateSOSRipples srcPoint
  EHC.liftFlow $ JB.animateCamera srcPoint.lat srcPoint.lng 16.0 "ZOOM"

getPoint :: API.GetDriverLocationResp -> CTA.Paths
getPoint (API.GetDriverLocationResp resp) = { lat: resp ^. _lat, lng: resp ^. _lon }