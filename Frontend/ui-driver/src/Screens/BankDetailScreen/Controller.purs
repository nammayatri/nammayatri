module Screens.BankDetailScreen.Controller where

import Prelude (Unit, bind, pure, ($), class Show, unit, (==), discard)
import Effect (Effect)
import PrestoDOM (Eval, Props, continue, exit)
import Screens.Types (BankDetailScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Engineering.Helpers.Commons (getNewIDWithTag)
import JBridge (disableActionEditText)
import Components.RegistrationModal.Controller as RegistrationModalController
import Components.OnboardingHeader.Controller as OnboardingHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen BANK_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen BANK_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen BANK_DETAILS_SCREEN)
    BeneficiaryNumber str -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "in_screen" "enter_benificiary_number"
    ReEnterBeneficiaryNumber str -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "in_screen" "re-enter_benificiary_number"
    IFSCNumber str -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "in_acreen" "ifsc_number"
    OnboardingHeaderAction act -> case act of
      OnboardingHeaderController.TriggerRegModal -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "onboarding_header" "trigger_registration_modal"
      OnboardingHeaderController.BackPressed -> do
        trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "onboarding_header" "backpressed"
        trackAppEndScreen appId (getScreen BANK_DETAILS_SCREEN)
    RegistrationModalAction (RegistrationModalController.OnCloseClick) -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "registration_modal" "on_close_click"
    PrimaryButtonAction act -> case act of
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen BANK_DETAILS_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen BANK_DETAILS_SCREEN) "primary_button" "no_action"
    Dummy -> trackAppScreenEvent appId (getScreen BANK_DETAILS_SCREEN) "in_screen" "dummy_action"
    NoAction -> trackAppScreenEvent appId (getScreen BANK_DETAILS_SCREEN) "in_screen" "no_action"


data ScreenOutput = GoToAddVehicleDetails BankDetailScreenState | GoBack

data Action =   Dummy | BackPressed | NoAction
  | BeneficiaryNumber String
  | ReEnterBeneficiaryNumber String
  | IFSCNumber String
  | OnboardingHeaderAction OnboardingHeaderController.Action
  | RegistrationModalAction RegistrationModalController.Action
  | PrimaryButtonAction PrimaryButtonController.Action
  | AfterRender

eval :: Action -> BankDetailScreenState -> Eval Action ScreenOutput BankDetailScreenState
eval AfterRender state = continue state
eval BackPressed state = exit GoBack
eval (OnboardingHeaderAction (OnboardingHeaderController.TriggerRegModal)) state = do --{props{openRegistrationModal = true}}
  continue state { props = state.props { openRegistrationModal = true } }
eval (BeneficiaryNumber val) state = do 
  continue state {data { beneficiaryNumber = val }}
eval (ReEnterBeneficiaryNumber val) state = do 
  _ <- pure $ disableActionEditText (getNewIDWithTag "verifybeneficiary")
  continue state {props { isBeneficiaryMatching = if(val == state.data.beneficiaryNumber) then true else false }}
eval (IFSCNumber val) state = do 
  _ <- pure $ printLog "State" state
  continue state {data = state.data { ifsc = val }}
eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = exit (GoToAddVehicleDetails state)
eval (OnboardingHeaderAction (OnboardingHeaderController.BackPressed)) state = exit GoBack
eval (RegistrationModalAction (RegistrationModalController.OnCloseClick)) state = do 
  continue state { props = state.props { openRegistrationModal = false } }
eval _ state = continue state
overrides :: String -> (Action -> Effect Unit) -> BankDetailScreenState -> Props (Effect Unit)
overrides _ push state = []