{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultiModalFlow.MultiModalScreen.Controller where

import Common.Types.App as App
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Screens.MultiModalFlow.MultiModalScreen.ScreenData
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.String.CodeUnits (slice)
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PopUpModal.Controller as PopUpModalController
import Components.RateCard as RateCard
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Constants.Configs (getPolylineAnimationConfig)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Array as DA
import Data.Maybe( Maybe(..), fromMaybe, maybe)
import Data.Tuple
import Debug (spy)
import Data.String as DS
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Prelude
import Storage
import PrestoDOM (class Loggable, Eval, BottomSheetState(..), update, continue, exit, continueWithCmd, updateAndExit)
import Services.API as API
import Screens.Types as ST
import Screens.Types (ParcelDeliveryScreenData(..))
import Data.Either (Either(..))
import Services.Backend as Remote
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action 
  = NoAction
  | MapViewLoaded String String String
  | BackPressed
  | ScrollStateChanged String

data ScreenOutput
  = GoToHomeScreen ST.MultiModalScreenState
  | RefreshScreen ST.MultiModalScreenState

eval :: Action -> ST.MultiModalScreenState -> Eval Action ScreenOutput ST.MultiModalScreenState

eval BackPressed state = 
  case state.data.currentStage of
   ST.MULTI_MODAL_RIDE_DETAILS -> updateAndExit state $ GoToHomeScreen state
   _ -> continue state

eval (MapViewLoaded _ _ _) state =
  continueWithCmd state [do
    void $ pure $ JB.removeAllPolylines ""
    -- let srcLat = state.data.sourceLat
    --     srcLon = state.data.sourceLong 
    --     dstLat = state.data.destinationLat
    --     dstLon = state.data.destinationLong
    --     markers = HU.normalRoute ""
    --     srcMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker }
    --     destMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker}
    -- void $ launchAff $ EHC.flowRunner defaultGlobalState $ do 
    --   let Tuple newRoute points = 
    --         case state.data.route of 
    --           Just (API.Route route) ->
    --             let (API.Snapped routePts) = route.points 
    --                 newPts = if DA.length routePts > 1 then 
    --                           JB.getExtendedPath $ Remote.walkCoordinates (route.points)
    --                           else 
    --                             Remote.walkCoordinate srcLat srcLon dstLat dstLon
    --                 newRoute = route {points = API.Snapped (map (\item -> API.LatLong { lat : item.lat, lon : item.lng}) newPts.points)}
    --             in Tuple newRoute newPts
    --           Nothing -> 
    --             let newPts = Remote.walkCoordinate srcLat srcLon dstLat dstLon
    --                 newRoute = {boundingBox: Nothing, distance: 0, duration: 0, pointsForRentals: Nothing, points : API.Snapped (map (\item -> API.LatLong { lat : item.lat, lon : item.lng}) newPts.points), snappedWaypoints : API.Snapped []}
    --             in Tuple newRoute newPts
    --       routeConfig = JB.mkRouteConfig points srcMarkerConfig destMarkerConfig Nothing  "NORMAL" "LineString" true JB.DEFAULT $ specialLocationConfig "" "" false getPolylineAnimationConfig
    --   EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "ParcelDetailsMapView")
    pure NoAction
  ] 

eval (ScrollStateChanged scrollState) state = do
  let sheetState = case scrollState of 
              "1" -> ST.STATE_DRAGGING
              "2" -> ST.STATE_SETTLING
              "3" -> ST.STATE_EXPANDED
              "4" -> ST.STATE_COLLAPSED
              "5" -> ST.STATE_HIDDEN
              "6" -> ST.STATE_HALF_EXPANDED
              _ -> ST.STATE_HIDDEN
  continue state {props {currentSheetState = if sheetState == ST.STATE_EXPANDED then EXPANDED else state.props.currentSheetState, sheetState = Nothing}} --bottomSheetState = sheetState, 

eval _ state = update state

specialLocationConfig :: String -> String -> Boolean -> App.PolylineAnimationConfig -> JB.MapRouteConfig
specialLocationConfig srcIcon destIcon isAnim animConfig =
  JB.mapRouteConfig
    { sourceSpecialTagIcon = srcIcon
    , destSpecialTagIcon = destIcon
    , vehicleSizeTagIcon = HU.getVehicleSize unit
    , isAnimation = isAnim
    , autoZoom = true
    , polylineAnimationConfig = animConfig
    }