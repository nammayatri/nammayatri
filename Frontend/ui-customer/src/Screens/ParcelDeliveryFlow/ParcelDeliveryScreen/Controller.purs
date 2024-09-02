module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller where

import Common.Types.App as App
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.ParcelDeliveryInstruction.Controller as ParcelDeliveryInstructionController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PopUpModal.Controller as PopUpModalController
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
import Storage
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
  | ParcelDeliveryInstructionAC ParcelDeliveryInstructionController.Action
  | DeliveryDetailAction (PopUpModalController.Action)

data ScreenOutput
  = GoToHomeScreen ST.ParcelDeliveryScreenState
  | RefreshScreen ST.ParcelDeliveryScreenState
  | GoToSelectLocation ST.ParcelDeliveryScreenState

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

eval (ParcelDeliveryInstructionAC (ParcelDeliveryInstructionController.PrimaryButtonAC PrimaryButtonController.OnClick)) state = 
  exit $ GoToSelectLocation state

eval (DeliveryDetailAction (PopUpModalController.PersonMobile (PrimaryEditTextController.TextChanged valId newVal))) state =
  if state.data.deliveryDetailsInfo.currentState == ST.SenderModal then
    continue state { data { deliveryDetailsInfo { sendersDetails = state.data.deliveryDetailsInfo.sendersDetails { mobileNumber = newVal } } } }
  else
    continue state { data { deliveryDetailsInfo { receiversDetails = state.data.deliveryDetailsInfo.receiversDetails { mobileNumber = newVal } } } }


eval (DeliveryDetailAction (PopUpModalController.PersonName (PrimaryEditTextController.TextChanged valId newVal))) state =
  if state.data.deliveryDetailsInfo.currentState == ST.SenderModal then
    continue state { data { deliveryDetailsInfo { sendersDetails = state.data.deliveryDetailsInfo.sendersDetails { name = newVal } } } }
  else
    continue state { data { deliveryDetailsInfo { receiversDetails = state.data.deliveryDetailsInfo.receiversDetails { name = newVal } } } }


eval (DeliveryDetailAction (PopUpModalController.PersonAddress (PrimaryEditTextController.TextChanged valId newVal))) state =
  if state.data.deliveryDetailsInfo.currentState == ST.SenderModal then
    continue state { data { deliveryDetailsInfo { sendersDetails = state.data.deliveryDetailsInfo.sendersDetails { address = newVal } } } }
  else
    continue state { data { deliveryDetailsInfo { receiversDetails = state.data.deliveryDetailsInfo.receiversDetails { address = newVal } } } }


eval (DeliveryDetailAction (PopUpModalController.PersonInstruction (PrimaryEditTextController.TextChanged valId newVal))) state =
  if state.data.deliveryDetailsInfo.currentState == ST.SenderModal then
    continue state { data { deliveryDetailsInfo { sendersDetails = state.data.deliveryDetailsInfo.sendersDetails { instruction = if newVal == "" then Nothing else Just newVal } } } }
  else
    continue state { data { deliveryDetailsInfo { receiversDetails = state.data.deliveryDetailsInfo.receiversDetails { instruction = if newVal == "" then Nothing else Just newVal } } } }

eval (DeliveryDetailAction (PopUpModalController.CheckBoxClick)) state = do
  let userName = getValueToLocalStore USER_NAME
      mobileNumber = getValueToLocalStore MOBILE_NUMBER
      
  if state.data.deliveryDetailsInfo.currentState == ST.SenderModal then
    if state.data.deliveryDetailsInfo.initiatedAs == ST.Sender then
      continue state {data {deliveryDetailsInfo {initiatedAs = ST.Else}}}
    else continue state { data { deliveryDetailsInfo { initiatedAs = ST.Sender, sendersDetails = state.data.deliveryDetailsInfo.sendersDetails { name = userName, mobileNumber = mobileNumber } } } }
  else if state.data.deliveryDetailsInfo.initiatedAs == ST.Receiver then
    continue state {data {deliveryDetailsInfo {initiatedAs = ST.Else}}}
  else
    continue state { data { deliveryDetailsInfo { initiatedAs = ST.Receiver, receiversDetails = state.data.deliveryDetailsInfo.receiversDetails { name = userName, mobileNumber = mobileNumber } } } }
  
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