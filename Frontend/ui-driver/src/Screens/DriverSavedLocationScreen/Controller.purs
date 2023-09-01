module Screens.DriverSavedLocationScreen.Controller where

import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons (getNewIDWithTag, setText)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as NUM
import Data.Number as Num
import Data.String (length, trim)
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (runEffectFn5)
import Effect.Unsafe (unsafePerformEffect)
import JBridge (addMarker, animateCamera, getCurrentLatLong, getCurrentPosition, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, removeAllPolylines, requestLocation, showMarker, toggleBtnLoader)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender, trackAppScreenEvent)
import Prelude (class Show, Unit, bind, pure, unit, ($), (<), (==), (&&), (/=))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.DriverSavedLocationScreen.Transformer (getLocationArray)
import Screens.Types (DriverSavedLocationScreenState, Location(..), SavedLocationScreenType(..))
import Services.API (GetHomeLocationsRes(..), Prediction(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverSavedLocationScreen"
    BackPressed -> trackAppBackPress appId (getScreen DRIVER_SAVED_LOCATION_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "onclick"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "no_action"
    MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "string" "map_rendered"
    _ -> pure unit

data Action
  = BackPressed
  | AfterRender
  | NoAction
  | PrimaryButtonAC PrimaryButton.Action
  | GoToLocationModalAC GoToLocationModal.Action
  | OnTextChanged String
  | ConfirmLocEDT String
  | MAPREADY String String String
  | LocateOnMap
  | AutoCompleteAPI String Number Number
  | UpdateLocation String String String
  | ConfirmChangesAC PrimaryButton.Action
  | SuggestionClick Prediction
  | Respones GetHomeLocationsRes
  | Error ErrorResponse
  | PopUpModalAction PopUpModal.Action

data ScreenOutput
  = DriverSavedLocationScreen
  | GotoHomeScreen
  | GoBack
  | CallAutoComplete String DriverSavedLocationScreenState
  | UpdateConfirmLocation DriverSavedLocationScreenState
  | SaveLocation DriverSavedLocationScreenState
  | GetPlaceNameAPI DriverSavedLocationScreenState String
  | DeleteLocation DriverSavedLocationScreenState String
  | UpdateHomeLocation DriverSavedLocationScreenState String
  | ChangeView DriverSavedLocationScreenState

eval :: Action -> DriverSavedLocationScreenState -> Eval Action ScreenOutput DriverSavedLocationScreenState
eval BackPressed state =
  if state.props.confirmDelete then
    continue state { props { confirmDelete = false } }
  else if state.props.viewType == ADD_GO_TO_LOCATION then
    continue state { props { viewType = GO_TO_LIST } }
  else if state.props.viewType == LOCATE_ON_MAP then
    continue state { props { viewType = ADD_GO_TO_LOCATION } }
  else if state.props.viewType == CONFIRM_LOCATION then
    continue state { props { viewType = if state.props.fromEditButton then GO_TO_LIST else LOCATE_ON_MAP } }
  else if state.props.viewType == GO_TO_LIST then
    exit GoBack
  else
    exit GoBack

eval (UpdateLocation key lat lon) state = case NUM.fromString lat, NUM.fromString lon of
  Just latitute, Just longitute -> exit $ UpdateConfirmLocation state { data { saveLocationObject { position { lat = latitute, lon = longitute } } } }
  _, _ -> continue state

eval LocateOnMap state = do
  let
    _ = unsafePerformEffect $ runEffectFn5 locateOnMap true 0.0 0.0 "" []
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ toggleBtnLoader "" false
  exit $ ChangeView state { props { viewType = LOCATE_ON_MAP } }

eval (ConfirmLocEDT val) state = continue state { data { saveLocationObject { tag = val } } }

eval (OnTextChanged textVal) state =
  if length (trim textVal) < 3 then
    continue state
  else
    continueWithCmd state
      [ do
          location <- getCurrentLatLong
          pure $ AutoCompleteAPI textVal location.lat location.lng
      ]

eval (AutoCompleteAPI textVal lat lng) state = updateAndExit state $ CallAutoComplete textVal state { data { lat = lat, lon = lng } }

eval (MAPREADY key latitude longitude) state =
  continueWithCmd state
    [ do
        _ <- checkPermissionAndUpdateDriverMarker state
        pure AfterRender
    ]

eval (PrimaryButtonAC PrimaryButton.OnClick) state = case state.props.viewType of
  NO_GO_TO_ADDED -> continue state { props { viewType = ADD_GO_TO_LOCATION } }
  GO_TO_LIST -> continue state { props { viewType = ADD_GO_TO_LOCATION } }
  LOCATE_ON_MAP -> continue state { props { viewType = CONFIRM_LOCATION, fromEditButton = false } }
  _ -> continue state

eval (ConfirmChangesAC PrimaryButton.OnClick) state =
  if state.props.fromEditButton
    then do
    let id = state.props.selectedLocation
    exit $ UpdateHomeLocation state id -- TODO :: Call edit API
  else do
    _ <- pure $ spy "Location tag state" state
    exit $ SaveLocation state

eval (GoToLocationModalAC (GoToLocationModal.EditLocation loc)) state = do
  _ <- pure $ spy "gotolocationitem1" state
  _ <- pure $ spy "goToLocationItem2" loc
  _ <- pure $ setText (getNewIDWithTag "ConfirmLocEDT") loc.tag
  continue
    state
      { props { viewType = CONFIRM_LOCATION, selectedLocation = loc.id, fromEditButton = true }
      , data
        { saveLocationObject
          { position
            { place = loc.id
            , lat = loc.lat
            , lon = loc.lon
            }
          , tag = loc.tag
          , address = loc.address
          }
        }
      }

eval (GoToLocationModalAC (GoToLocationModal.DeleteLocation loc)) state = continue state { props { confirmDelete = true, selectedLocation = loc.id } }

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let
    id = state.props.selectedLocation
  if id /= "" then
    exit $ DeleteLocation state id
  else
    continue state

eval (PopUpModalAction PopUpModal.OnButton1Click) state = continue state { props { confirmDelete = false } }

eval (SuggestionClick pred) state = do
  let
    (Prediction prediction) = pred
  case prediction.placeId of
    Just id -> exit $ GetPlaceNameAPI state { props { selectedPrediction = pred } } id
    Nothing -> continue state

eval (Respones resp) state = do
  let tempArray = getLocationArray resp
  continue state { data { savedLocationsArray = tempArray } , props {viewType = if tempArray == [] then NO_GO_TO_ADDED else GO_TO_LIST } }

eval _ state = continue state

checkPermissionAndUpdateDriverMarker :: DriverSavedLocationScreenState -> Effect Unit
checkPermissionAndUpdateDriverMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  if conditionA && conditionB then do
    _ <- getCurrentPosition (showDriverMarker "") constructLatLong
    pure unit
  else do
    _ <- requestLocation unit
    pure unit

showDriverMarker :: String -> Location -> Effect Unit
showDriverMarker marker location = do
  _ <- showMarker marker location.lat location.lon 100 0.5 0.5
  animateCamera location.lat location.lon 15 "ZOOM"

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Num.fromString lat)
  , lon: fromMaybe 0.0 (Num.fromString lng)
  , place: ""
  }
