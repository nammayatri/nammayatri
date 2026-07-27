module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller where

import Common.Types.App as App
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData
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
import Common.Types.App
import Prelude
import Storage
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd, updateAndExit)
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
  | EditAddress Boolean
  | ExpandInstructions
  | GoBack
  | GenericHeaderAC GenericHeaderController.Action
  | MapViewLoaded String String String
  | PrimaryButtonActionController PrimaryButtonController.Action
  | DeliveryDetailAction (PopUpModalController.Action)
  | ChooseVehicleAC ChooseVehicleController.Action
  | PickContactCallBack String String
  | RateCardAction RateCard.Action
  | UpdateNameAndNumber String String Boolean
  | ValidateInputFields
  | Refresh
  | GoToSelectContactScreen

data ScreenOutput
  = GoToHomeScreen ST.ParcelDeliveryScreenState
  | RefreshScreen ST.ParcelDeliveryScreenState
  | GoToSelectLocation ST.ParcelDeliveryScreenState
  | GoToChooseYourRide ST.ParcelDeliveryScreenState
  | GoToConfirmgDelivery ST.ParcelDeliveryScreenState
  | GoToSelectContact ST.ParcelDeliveryScreenState

eval :: Action -> ST.ParcelDeliveryScreenState -> Eval Action ScreenOutput ST.ParcelDeliveryScreenState

eval GoBack state = 
  case state.data.currentStage of
   ST.SENDER_DETAILS -> updateAndExit state $ GoToChooseYourRide state
   ST.RECEIVER_DETAILS -> continue state { data { currentStage = ST.SENDER_DETAILS } }
   ST.FINAL_DETAILS -> updateAndExit state $ GoToChooseYourRide state
   ST.DELIVERY_INSTRUCTIONS -> if HU.isParentView FunctionCall then do 
                                  void $ pure $ HU.emitTerminateApp Nothing true
                                  continue state
                              else updateAndExit state $ GoToHomeScreen state

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
          routeConfig = JB.mkRouteConfig points srcMarkerConfig destMarkerConfig Nothing  "NORMAL" "LineString" true JB.DEFAULT $ specialLocationConfig "" "" false getPolylineAnimationConfig
      EHC.liftFlow $ JB.drawRoute [routeConfig] (EHC.getNewIDWithTag "ParcelDetailsMapView")
    pure NoAction
  ] 

eval (EditAddress isSource) state =
  if isSource == true
    then continue $ state {data {currentStage = ST.SENDER_DETAILS}, props { isEditModal = true}}
    else continue $ state {data {currentStage = ST.RECEIVER_DETAILS}, props { isEditModal = true}}


eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state =
  continueWithCmd state [pure GoBack]

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = 
  case state.data.currentStage of
    ST.FINAL_DETAILS -> exit $ GoToConfirmgDelivery state 
    ST.DELIVERY_INSTRUCTIONS -> exit $ GoToSelectLocation state
    _ -> continue state

eval (DeliveryDetailAction (PopUpModalController.PersonMobile (PrimaryEditTextController.TextChanged valId newVal))) state =
  continue $ state { props { editDetails { phone =  newVal }}}


eval (DeliveryDetailAction (PopUpModalController.PersonName (PrimaryEditTextController.TextChanged valId newVal))) state =
  continue $ state { props { editDetails { name = newVal }}}

eval (DeliveryDetailAction (PopUpModalController.PersonName PrimaryEditTextController.TextImageClicked)) state = do
  let updatedState = if state.data.currentStage == ST.SENDER_DETAILS then state {data { senderDetails { name = state.props.editDetails.name, phone = state.props.editDetails.phone}}} else state {data { receiverDetails { name = state.props.editDetails.name, phone = state.props.editDetails.phone}}}
  continueWithCmd updatedState $ [do pure $ GoToSelectContactScreen]

eval (DeliveryDetailAction (PopUpModalController.PersonMobile PrimaryEditTextController.TextImageClicked)) state = do
  let updatedState = if state.data.currentStage == ST.SENDER_DETAILS then state {data { senderDetails { name = state.props.editDetails.name, phone = state.props.editDetails.phone}}} else state {data { receiverDetails { name = state.props.editDetails.name, phone = state.props.editDetails.phone}}}
  continueWithCmd updatedState $ [do pure $ GoToSelectContactScreen]
  
eval GoToSelectContactScreen state = updateAndExit state $ GoToSelectContact state

eval (DeliveryDetailAction (PopUpModalController.PersonAddress (PrimaryEditTextController.TextChanged valId newVal))) state = do
  continue $ state { props { editDetails { extras = newVal } } }

eval (DeliveryDetailAction (PopUpModalController.PersonInstruction (PrimaryEditTextController.TextChanged valId newVal))) state = do
  continue $ state { props { editDetails { instructions = if newVal == "" then Nothing else Just newVal } } }

eval (DeliveryDetailAction (PopUpModalController.CheckBoxClick)) state = do
  let userName = getValueToLocalStore USER_NAME
      mobileNumber = getValueToLocalStore MOBILE_NUMBER

  if state.data.currentStage == ST.SENDER_DETAILS then
    if state.data.initiatedAs == API.Sender then
      continue state {data { initiatedAs = API.SomeoneElse}}
    else 
      let newState = state { props { editDetails { name = userName, phone =  mobileNumber} }, data { initiatedAs = API.Sender, senderDetails {name = "", phone = ""}} }
      in continueWithCmd newState $ [do pure $ UpdateNameAndNumber userName mobileNumber true]
  else if state.data.initiatedAs == API.Receiver then
    continue state { data {initiatedAs = API.SomeoneElse}}
  else
    let newState = state {  props { editDetails{ name = userName, phone =  mobileNumber} }, data { initiatedAs = API.Receiver, receiverDetails {name = "", phone = ""} } }
    in continueWithCmd newState $ [do pure $ UpdateNameAndNumber userName mobileNumber false]

eval (DeliveryDetailAction (PopUpModalController.OnButton1Click)) state = do
  if state.data.currentStage == ST.SENDER_DETAILS then do
    if state.props.isEditModal then 
      continue state { data { currentStage = ST.FINAL_DETAILS}, props { isEditModal = false}} 
    else 
      updateAndExit state $ GoToChooseYourRide state
  else do
    if state.props.isEditModal then continue state { data { currentStage = ST.FINAL_DETAILS }, props { isEditModal = false}} else continue $ state { props { editDetails = state.data.senderDetails}, data { currentStage = ST.SENDER_DETAILS, receiverDetails = state.props.editDetails } }
    

eval (DeliveryDetailAction (PopUpModalController.OnButton2Click)) state = do
  if not $ validateInput state then
    continue state
  else if state.data.currentStage == ST.SENDER_DETAILS then do
    let nextStage = if state.props.isEditModal then ST.FINAL_DETAILS else ST.RECEIVER_DETAILS 
        newState = state { props { editDetails = state.data.receiverDetails, isEditModal = false}, data { currentStage = nextStage, senderDetails = state.props.editDetails, receiverDetails { instructions = if state.data.receiverDetails.instructions == Nothing then Just " " else state.data.receiverDetails.instructions } }}
    continueWithCmd newState $ [do pure Refresh] 
  else do
    let newState = state { data { currentStage = ST.FINAL_DETAILS, receiverDetails = state.props.editDetails } }
    continue newState

eval (PickContactCallBack userName contactNo) state = do
  let eiRegexPattern = regex "\\D" global
      formattedNumber = case eiRegexPattern of
        Right regexPattern -> replace regexPattern "" contactNo
        Left _ -> contactNo

  if state.data.currentStage == ST.SENDER_DETAILS then
    let newState = state { props { editDetails { name = userName, phone =  formattedNumber} }, data { senderDetails { name = "", phone =  "" } } }
    in continueWithCmd newState $ [do pure $ UpdateNameAndNumber userName formattedNumber true]
  else
    let newState = state {  props { editDetails { name = userName, phone =  formattedNumber} }, data { receiverDetails { name = "", phone =  "" } } }
    in continueWithCmd newState $ [do pure $ UpdateNameAndNumber userName formattedNumber false]

eval (ChooseVehicleAC (ChooseVehicleController.ShowRateCard config)) state = do
  continue state{ props { showRateCard = true }
                , data {  rateCard {  onFirstPage = false
                                    , currentRateCardType = App.DefaultRateCard
                                    , extraFare = config.extraFare
                                    , fareInfoDescription = config.fareInfoDescription
                                    , additionalFare = config.additionalFare
                                    , isNightShift = config.isNightShift
                                    , nightChargeTill = config.nightChargeTill
                                    , nightChargeFrom = config.nightChargeFrom
                                    , driverAdditions = config.driverAdditions
                                    , serviceTierName = config.serviceTierName
                                    }}}

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = App.DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = App.DefaultRateCard}}}

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = App.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = App.DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToFareUpdate) state = continue state { data{rateCard{currentRateCardType = App.FareUpdate,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToWaitingCharges) state = continue state { data{rateCard{currentRateCardType = App.WaitingCharges,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = App.TollOrParkingCharges,onFirstPage = true}}}

eval (UpdateNameAndNumber name number isSender) state = 
  if isSender then continue state { data { senderDetails { name = name, phone = number } } }
  else continue state { data { receiverDetails { name = name, phone = number } } }

eval Refresh state = exit $ RefreshScreen state { data { receiverDetails { name = if state.data.receiverDetails.name == " " then "" else state.data.receiverDetails.name, phone = if state.data.receiverDetails.phone == " " then "" else state.data.receiverDetails.phone, extras = if state.data.receiverDetails.extras == " " then "" else state.data.receiverDetails.extras, instructions = if state.data.receiverDetails.instructions == Just " " then Nothing else state.data.receiverDetails.instructions } } }

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

validateInput :: ST.ParcelDeliveryScreenState -> Boolean
validateInput state = 
  let
    details = state.props.editDetails
    firstLetter = slice 0 1 details.phone
    isValidNumber = firstLetter >= "6" && firstLetter <= "9"
    isValidInputs = DS.length details.name > 2 && DS.length details.phone == 10 && isValidNumber && DS.length details.extras > 2
  in
    isValidInputs

