module Screens.ApplicationStatusScreen.Controller where

import Prelude (class Show, pure, unit, bind, ($), discard, (==), (&&))
import PrestoDOM (Eval, continue, exit,continueWithCmd)
import Screens.Types (ApplicationStatusScreenState)
import PrestoDOM.Types.Core (class Loggable)
import JBridge (openWhatsAppSupport, minimizeApp)
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Services.APITypes(DriverRegistrationStatusResp(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APPLICATION_STATUS_SCREEN)
    BackPressed -> trackAppBackPress appId (getScreen APPLICATION_STATUS_SCREEN)
    PrimaryButtonActionController -> do
      trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "primary_button" "on_click"
      trackAppEndScreen appId (getScreen APPLICATION_STATUS_SCREEN)
    Logout -> do
      trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "Logout"
      trackAppEndScreen appId (getScreen APPLICATION_STATUS_SCREEN)
    SupportCall -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "whats_app_support_call"
    DriverRegistrationStatusAction resp -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "driver_registration_status"
    Dummy -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "dummy"
    ReTry docType -> case docType of
      "DL" -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "DL_retry_on_click"
      "RC" -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "RC_retry_on_click"
      _ -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "retry_on_click"

data ScreenOutput = GoToHomeScreen | LogoutAccount | GoToDlScreen | GoToVehicleDetailScreen

data Action = BackPressed 
              | PrimaryButtonActionController 
              | Logout 
              | SupportCall 
              | Dummy 
              | AfterRender 
              | DriverRegistrationStatusAction DriverRegistrationStatusResp
              | ReTry String


eval :: Action -> ApplicationStatusScreenState -> Eval Action ScreenOutput ApplicationStatusScreenState
eval AfterRender state = continue state
eval BackPressed state = do
  _ <- pure $ minimizeApp ""
  continue state
eval (PrimaryButtonActionController) state = exit GoToHomeScreen
eval (ReTry docType) state = case docType of
                                "DL" -> exit GoToDlScreen
                                "RC" -> exit GoToVehicleDetailScreen
                                _ -> continue state                               
eval Logout state = exit LogoutAccount
eval SupportCall  state = continueWithCmd state [do
  _ <- liftEffect $ openWhatsAppSupport "+918618963188"
  pure Dummy
  ]
eval (DriverRegistrationStatusAction (DriverRegistrationStatusResp resp)) state = do
  if (resp.dlVerificationStatus == "VALID" && resp.rcVerificationStatus == "VALID") then 
    exit GoToHomeScreen
    else continue state { data { dlVerificationStatus = resp.dlVerificationStatus, rcVerificationStatus = resp.rcVerificationStatus}}
eval _ state = continue state
