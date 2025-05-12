module Screens.ChooseCityScreen.Controller where

import Components.GenericHeader as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SelectMenuButton.Controller (Action(..)) as MenuButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Number as Number
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Helpers.Utils (getDistanceBwCordinates)
import JBridge (firebaseLogEvent, getCurrentLatLong, isLocationPermissionEnabled, minimizeApp, requestLocation)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import MerchantConfig.Types (CityConfig)
import Prelude (class Show, bind, pure, ($), (==), (||), unit, not, discard, (<), (&&), (<=))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Components.ErrorModal.Controller as ErrorModalController
import Locale.Utils

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "ChooseCityScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action"
    _ -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action" --change this


data Action = BackPressed
            | AfterRender
            | PrimaryButtonAC PrimaryButtonController.Action
            | MenuButtonAction MenuButtonController.Action
            | GenericHeaderAC GenericHeaderController.Action
            | ChangeStage ChooseCityScreenStage
            | LocationPermissionCallBack Boolean
            | UpdatePermission ChooseCityScreenState
            | UpdateLocationPermissionState
            | NoAction
            | CurrentLocationCallBack String String String
            | IsMockLocation String
            | ErrorModalActionController ErrorModalController.Action
            -- | MenuButtonAction2 MenuButtonController.Action

data ScreenOutput = WelcomeScreen 
                    | SelectLanguageScreen 
                    | RefreshScreen ChooseCityScreenState
                    | DetectCityAPI Number Number ChooseCityScreenState
                    | GoToAddVehicles ChooseCityScreenState

eval :: Action -> ChooseCityScreenState -> Eval Action ScreenOutput ChooseCityScreenState

eval AfterRender state = if state.props.currentStage == ENABLE_PERMISSION then continueWithCmd state [ do 
                            isLocationPermission <- isLocationPermissionEnabled unit
                            pure $ UpdatePermission state{ props { isLocationPermissionGiven = isLocationPermission, currentStage = if isLocationPermission then DETECT_LOCATION else state.props.currentStage}}]
                            else continue state
eval BackPressed state = do 
  if state.props.goBackToAddVehiclesScreen then exit $ GoToAddVehicles state {props {goBackToAddVehiclesScreen = false}}
  else do
    case state.props.currentStage of
      _ | state.props.currentStage == DETECT_LOCATION || state.props.currentStage == ENABLE_PERMISSION -> do
                                                                                                            _ <- pure $ minimizeApp ""
                                                                                                            continue state --minimizeApp
      _ | state.props.currentStage == SELECT_CITY || state.props.currentStage == SELECT_LANG -> continue state{props{currentStage = DETECT_LOCATION}}
      _ -> continue state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  if state.props.currentStage == ENABLE_PERMISSION then do
            if not(state.props.isLocationPermissionGiven) then do
              continueWithCmd state [do
                  isLocationPermission <- isLocationPermissionEnabled unit
                  if (isLocationPermission) then do
                      pure UpdateLocationPermissionState
                      else do 
                      _ <- liftEffect $ requestLocation unit
                      pure NoAction
                  ]
              else continue state
    else if state.props.currentStage == SELECT_CITY then do
      let newState = state {props {currentStage = DETECT_LOCATION}, data{ locationSelected = Just state.props.radioMenuFocusedCity}}
      if state.props.goBackToAddVehiclesScreen then exit $ GoToAddVehicles newState else continue newState
    else if state.props.currentStage == SELECT_LANG then do
      let _ = setLanguageLocale state.props.radioMenuFocusedLang
      continue state {props{currentStage = DETECT_LOCATION, selectedLanguage =  state.props.radioMenuFocusedLang}}
    else do
      case state.data.locationSelected of 
        Just location -> do
          _ <- pure $ setValueToLocalStore DRIVER_LOCATION location
          let mbCity = DA.find (\city' -> city'.cityName == location) state.data.config.cityConfig
          case mbCity of 
            Just city -> do
              _ <- pure $ setValueToLocalStore SHOW_SUBSCRIPTIONS if city.showSubscriptions then "true" else "false"
              pure unit
            Nothing -> pure unit
          exit WelcomeScreen
        Nothing -> continue state
          
      

eval (MenuButtonAction (MenuButtonController.OnSelection btnState)) state = 
  if state.props.currentStage == SELECT_CITY then
    continue state { props { radioMenuFocusedCity = btnState.text.value }}
  else
    continue state { props { radioMenuFocusedLang = btnState.text.value }}


eval (ChangeStage newStage) state = continue state{props{currentStage = newStage}, data {locationSelected = state.data.locationSelected}}

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = if state.props.goBackToAddVehiclesScreen then exit $ GoToAddVehicles state else continue state{props{currentStage = DETECT_LOCATION}}

eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do
  if isLocationPermissionEnabled then do
    let newState =  state { props {isLocationPermissionGiven = true, currentStage = DETECT_LOCATION }}
    updateAndExit newState $ RefreshScreen newState
  else continue state

eval UpdateLocationPermissionState state = do
  let newState = state {props {isLocationPermissionGiven = true, currentStage = DETECT_LOCATION}}
  updateAndExit newState $ RefreshScreen newState

eval (IsMockLocation isMock) state = do
  let val = isMock == "true"
      _ = unsafePerformEffect $ if val then logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state { props { isMockLocation = isMock == "true", locationDetectionFailed = false } }

eval (UpdatePermission updatedState) state = do
  let newState = updatedState {props {currentStage = if updatedState.props.isLocationPermissionGiven then DETECT_LOCATION else updatedState.props.currentStage}}
  continue newState

eval (CurrentLocationCallBack lat long _) state = do
  let driverLat = fromMaybe 0.0 $ Number.fromString lat
      driverLon = fromMaybe 0.0 $ Number.fromString long
      newState = state { props { lat = driverLat, lon = driverLon}}
  if driverLat == 0.0 && driverLon == 0.0 then
    continue state {props{ locationDetectionFailed = true}}
  else if driverLat == state.props.lat && driverLon == state.props.lon then continue state
  else updateAndExit newState $ DetectCityAPI driverLat driverLon newState

eval _ state = update state