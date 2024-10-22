{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.MultiModalFlow.JourneyTrackingScreen.Controller where

import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Debug
import Prelude 
import PrestoDOM 
import Screens.MultiModalFlow.JourneyTrackingScreen.ScreenData
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
import Screens.MultiModalFlow.Components.MetroCard as MetroCard
import Screens.MultiModalFlow.Components.AlertWidget as AlertWidget
import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
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

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data ScreenOutput
  = Exit ST.JourneyTrackingScreenState
  | AddEmergencyContacts ST.JourneyTrackingScreenState
  | UpdateEmergencyContacts ST.JourneyTrackingScreenState
  | GoToSafetyDrill ST.JourneyTrackingScreenState

data Action
  = AfterRender
  | BackPressed
  | MetroCardAction MetroCard.Action
  | VehicleCardAction VehicleCard.Action
  | MapReady String String String
  | NoAction
  | AlertWidgetAction AlertWidget.Action
  | ShareRide
  | ToggleViewButtonClicked

eval :: Action -> ST.JourneyTrackingScreenState -> Eval Action ScreenOutput ST.JourneyTrackingScreenState

eval (MetroCardAction MetroCard.ExpandStops) state = continue state

eval (VehicleCardAction VehicleCard.ExpandStops) state = continue state

eval (AlertWidgetAction AlertWidget.NoAction) state = continue state

eval (MapReady _ _ _ )state = continueWithCmd state [do 
  void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
    defaultMockInviteFlow 0 state 
    let _ = spy "defaultMockInviteFlow" "defaultMockInviteFlow"
    pure unit
  pure NoAction
]

-- eval Backpressed state =


eval _ state = update state


drawDriverRoute :: ST.DriverInfoCard -> CTA.Paths -> API.Route -> Boolean -> Flow GlobalState Unit
drawDriverRoute ride srcPoint (API.Route route) showRipples = do
  let markers = HU.normalRoute ""
      srcMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, primaryText = "getString SOS_LOCATION" }
      
      destMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = "getString DROP" }
  let routeConfig = JB.mkRouteConfig (Remote.walkCoordinates route.points) srcMarkerConfig destMarkerConfig Mb.Nothing "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig
  EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "JourneyTrackingScreenMap")
  -- void $ runExceptT $ runBackT $ Remote.drawMapRoute srcPoint.lat srcPoint.lng ride.destinationLat ride.destinationLng srcMarkerConfig destMarkerConfig "NORMAL" route "trip" $ (HSConfig.specialLocationConfig "" "" false getPolylineAnimationConfig) { autoZoom = false }
  -- when showRipples $ do
  --   EHC.liftFlow $ addAndUpdateSOSRipples srcPoint
  EHC.liftFlow $ JB.animateCamera srcPoint.lat srcPoint.lng 16.0 "ZOOM"


defaultMockInviteFlow :: Int -> ST.JourneyTrackingScreenState -> Flow GlobalState Unit
defaultMockInviteFlow id state = do
  -- localDelay 1000.0
  let
    srcPoint = getPoint mockDriverLocation
    _ = spy "srcPoint" srcPoint
  -- pushAction $ UpdateMockData mockDriverInfo { vehicleDetails = "AUTO_RICKSHAW", vehicleVariant = "AUTO_RICKSHAW"}
  drawDriverRoute mockDriverInfo srcPoint mockRoute false

getPoint :: API.GetDriverLocationResp -> CTA.Paths
getPoint (API.GetDriverLocationResp resp) = { lat: resp ^. _lat, lng: resp ^. _lon }