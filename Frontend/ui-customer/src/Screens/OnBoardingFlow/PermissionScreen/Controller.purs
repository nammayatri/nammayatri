module Screens.PermissionScreen.Controller where

import Components.ErrorModal.Controller as ErrorModalController
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge (isInternetAvailable, requestLocation)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, bind, pure, unit, (==), discard, ($))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (PermissionScreenState)

instance showAction :: Show Action where 
    show _ = ""
  
instance loggableAction :: Loggable Action where 
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen PERMISSION_SCREEN)
      BackPressed -> do
        trackAppBackPress appId (getScreen PERMISSION_SCREEN)
        trackAppEndScreen appId (getScreen PERMISSION_SCREEN)
      ErrorModalActionController act -> case act of
        ErrorModalController.PrimaryButtonActionController act -> case act of 
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "error_modal_action" "primary_button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "error_modal_action" "primary_button_no_action"
      PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "primary_button" "grant_access"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "primary_button" "no_action"
      Reload -> do
        trackAppActionClick appId (getScreen PERMISSION_SCREEN) "in_screen" "reload"
        trackAppEndScreen appId (getScreen PERMISSION_SCREEN)
      InternetCallBackCustomer str -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "internet_call_back_customer"
      LocationPermissionCallBackCustomer str -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "location_permission_call_back_customer"
      NoAction -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "no_action"

data Action = ErrorModalActionController ErrorModalController.Action 
            | PrimaryButtonActionController PrimaryButtonController.Action
            | NoAction
            | Reload
            | BackPressed
            | LocationPermissionCallBackCustomer String
            | InternetCallBackCustomer String
            | AfterRender

data ScreenOutput = GoBack | Refresh | InternetCallBack PermissionScreenState | LocationCallBack PermissionScreenState 

eval :: Action -> PermissionScreenState -> Eval Action ScreenOutput PermissionScreenState

eval BackPressed state = exit GoBack 

eval (ErrorModalActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  continueWithCmd state [do 
    conditionA <- isInternetAvailable unit
    if conditionA then do
      pure Reload
      else do
        pure NoAction
  ]

eval (LocationPermissionCallBackCustomer isLocationPermissionEnabled) state = do 
  if isLocationPermissionEnabled == "true" then do 
    updateAndExit state (LocationCallBack state)
    else continue state
eval (InternetCallBackCustomer isInternetAvailable) state = do 
  if( isInternetAvailable == "true") then do
    updateAndExit state (InternetCallBack state)
    else continue state

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = continueWithCmd state [ do 
  _ <- requestLocation unit
  pure NoAction
  ]

eval Reload state = updateAndExit state $ Refresh
  
eval _ state = continue state
