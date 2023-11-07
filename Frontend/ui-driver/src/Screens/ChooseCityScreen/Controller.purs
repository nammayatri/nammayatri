module Screens.ChooseCityScreen.Controller where

import Components.GenericHeader as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SelectMenuButton.Controller (Action(..)) as MenuButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import JBridge (minimizeApp, firebaseLogEvent, isLocationPermissionEnabled, requestLocation, minimizeApp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($), (==), (||), unit, not, discard)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)

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
            | MenuButtonAction2 MenuButtonController.Action

data ScreenOutput = WelcomeScreen | SelectLanguageScreen 

eval :: Action -> ChooseCityScreenState -> Eval Action ScreenOutput ChooseCityScreenState

eval AfterRender state = if state.props.currentStage == ENABLE_PERMISSION then continueWithCmd state [ do 
                            isLocationPermission <- isLocationPermissionEnabled unit
                            _<- pure $ spy "test1 : " isLocationPermission
                            pure $ UpdatePermission state{ props { isLocationPermissionGiven = isLocationPermission, currentStage = if isLocationPermission then DETECT_LOCATION else state.props.currentStage}}]
                            else continue state
eval BackPressed state = do 
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
                      _ <- pure $ spy "testing5 : " "abc"  
                      pure UpdateLocationPermissionState
                      else do 
                      _ <- pure $ spy "testing6 : " "abc" 
                      _ <- liftEffect $ requestLocation unit
                      pure NoAction
                  ]
              else continue state
    -- continue state{props{currentStage = DETECT_LOCATION}}
    -- else if state.props.currentStage == DETECT_LOCATION then continue state{props{currentStage = CAROUSEL}}
    else if state.props.currentStage == SELECT_CITY then do
      _ <- pure $ setValueToLocalStore DRIVER_LOCATION state.data.locationSelected
      let mbCity = DA.find (\city' -> city'.cityName == state.data.locationSelected) state.data.config.cityConfig
      case mbCity of 
        Just city -> do
          _ <- pure $ setValueToLocalStore SHOW_SUBSCRIPTIONS if city.showSubscriptions then "true" else "false"
          pure unit
        Nothing -> pure unit
      continue state{props{currentStage = DETECT_LOCATION}}
    else if state.props.currentStage == SELECT_LANG then do
      _ <- pure $ setValueToLocalStore LANGUAGE_KEY state.props.selectedLanguage
      continue state {props{currentStage = DETECT_LOCATION}}
    else exit WelcomeScreen

eval (MenuButtonAction (MenuButtonController.OnSelection btnState)) state = continue state { props { selectedLanguage = btnState.text.value }}

eval (MenuButtonAction2 (MenuButtonController.OnSelection btnState)) state = continue state { data { locationSelected = btnState.text.value }}

eval (ChangeStage newStage) state = continue state{props{currentStage = newStage}}

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = continue state{props{currentStage = DETECT_LOCATION}}

eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do
--   _ <- pure $ spy "location permission" isLocationPermissionEnabled
  if isLocationPermissionEnabled then do
    _ <- pure $ spy "location permission" isLocationPermissionEnabled
    continue state { props {currentStage = DETECT_LOCATION }}
  else continue state
    -- let _ = unsafePerformEffect $ logEvent state.data.logField  "permission_granted_location"
    -- continue state {props {isLocationPermissionGiven = isLocationPermissionEnabled, currentStage = if state.props.isLocationPermissionGiven then DETECT_LOCATION else state.props.currentStage}}
    -- else continue state {props {isLocationPermissionGiven = isLocationPermissionEnabled, currentStage = if state.props.isLocationPermissionGiven then DETECT_LOCATION else state.props.currentStage}}

eval UpdateLocationPermissionState state = continue state {props {isLocationPermissionGiven = true, currentStage = DETECT_LOCATION}}

eval (UpdatePermission updatedState) state = do
  _ <- pure $ spy "testing " updatedState
  continue updatedState {props {currentStage = if updatedState.props.isLocationPermissionGiven then DETECT_LOCATION else updatedState.props.currentStage}}

eval _ state = continue state