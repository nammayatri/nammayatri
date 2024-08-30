module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller where

import Common.Types.App as App
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Constants.Configs (getPolylineAnimationConfig)
import Data.Array as DA
import Data.Maybe( Maybe(..), fromMaybe, maybe)
import Data.Tuple
import Debug (spy)
import Effect.Aff (launchAff)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Prelude
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd)
import Services.API as API
import Screens.Types as ST
import Services.Backend as Remote
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action 
  = NoAction
  | EditAddress Boolean
  | ExpandInstructions
  | GoBack
  | GenericHeaderAC GenericHeaderController.Action
  | MapViewLoaded String String String
  | PrimaryButtonActionController PrimaryButtonController.Action

data ScreenOutput
  = GoToHomeScreen ST.ParcelDeliveryScreenState
  | RefreshScreen ST.ParcelDeliveryScreenState

eval :: Action -> ST.ParcelDeliveryScreenState -> Eval Action ScreenOutput ST.ParcelDeliveryScreenState

eval GoBack state = exit $ GoToHomeScreen state

eval (MapViewLoaded _ _ _) state =
  continueWithCmd state [do
    void $ pure $ JB.removeAllPolylines ""
    let srcLat = state.data.sourceLat
        srcLon = state.data.sourceLong 
        dstLat = state.data.destinationLat
        dstLon = state.data.destinationLong
        markers = HU.normalRoute ""
        srcMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker }
        destMarkerConfig = JB.defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker}
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ do 
      let Tuple newRoute points = 
            case state.data.route of 
              Just (API.Route route) ->
                let (API.Snapped routePts) = route.points 
                    newPts = if DA.length routePts > 1 then 
                              JB.getExtendedPath $ Remote.walkCoordinates (route.points)
                              else 
                                Remote.walkCoordinate srcLat srcLon dstLat dstLon
                    newRoute = route {points = API.Snapped (map (\item -> API.LatLong { lat : item.lat, lon : item.lng}) newPts.points)}
                in Tuple newRoute newPts
              Nothing -> 
                let newPts = Remote.walkCoordinate srcLat srcLon dstLat dstLon
                    newRoute = {boundingBox: Nothing, distance: 0, duration: 0, pointsForRentals: Nothing, points : API.Snapped (map (\item -> API.LatLong { lat : item.lat, lon : item.lng}) newPts.points), snappedWaypoints : API.Snapped []}
                in Tuple newRoute newPts
          _ = spy "markers" markers
          _ = spy "points" points
          routeConfig = JB.mkRouteConfig points srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT $ specialLocationConfig "" "" false getPolylineAnimationConfig
      EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "ParcelDeliveryScreenMap")
    pure NoAction
  ] 

eval (EditAddress isSource) state =
  if isSource == true
    then continue state {data {currentStage = ST.SENDER_DETAILS}}
    else continue state {data {currentStage = ST.RECEIVER_DETAILS}}


eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state =
  continueWithCmd state [pure GoBack]

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = 
  exit $ GoToHomeScreen state

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