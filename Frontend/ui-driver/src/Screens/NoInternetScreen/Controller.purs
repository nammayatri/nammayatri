module Screens.NoInternetScreen.Controller where

import Prelude(class Show, bind, pure, unit, (==))
import Screens.Types (NoInternetScreenState)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge (isInternetAvailable,requestLocation)
import Screens(ScreenName(..), getScreen)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)

instance showAction :: Show Action where 
    show _ = ""
  
instance loggableAction :: Loggable Action where 
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen NO_INTERNET_SCREEN)
      BackPressed -> do
        trackAppBackPress appId (getScreen NO_INTERNET_SCREEN)
      PrimaryButtonActionController triggertype act-> do
        case triggertype of
          "LOCATION_DISABLED" -> case act of
            PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "grantaccess_on_click"
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "grantaccess_no_action"
          "INTERNET_ACTION" -> case act of
            PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "tryagain_on_click"
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "tryagain_no_action"
          _ -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "primary_button" "no_action"
      LocationPermissionCallBack isLocationPermissionEnabled -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "location_permission_callback"
      InternetActionCallBack isInternetAvailable -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "internet_action_callback"
      Reload -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "reload"
      NoAction -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "no_action"

data Action = PrimaryButtonActionController String PrimaryButtonController.Action
            | NoAction
            | Reload
            | BackPressed
            | LocationPermissionCallBack String
            | InternetActionCallBack String
            | AfterRender

data ScreenOutput = GoBack | Refresh | InternetCallBack NoInternetScreenState | LocationCallBack NoInternetScreenState 

eval :: Action -> NoInternetScreenState -> Eval Action ScreenOutput NoInternetScreenState

eval BackPressed state = exit GoBack 


eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do 
  if isLocationPermissionEnabled == "true" then do 
    exit (LocationCallBack state)
    else continue state

eval (InternetActionCallBack isInternetAvailable) state = do 
  if( isInternetAvailable == "true") then do
    exit (InternetCallBack state)
    else continue state

eval (PrimaryButtonActionController triggertype PrimaryButtonController.OnClick) state = 
  if (triggertype == "LOCATION_DISABLED") then 
    continueWithCmd state [do 
      _ <- requestLocation unit
      pure NoAction]
      else continueWithCmd state [do 
      internetCondition <- isInternetAvailable unit
      if internetCondition then pure Reload else pure NoAction
      ]

eval Reload state = exit Refresh

eval _ state = continue state
