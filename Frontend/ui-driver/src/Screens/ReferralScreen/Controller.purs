module Screens.ReferralScreen.Controller where

import Prelude (bind , class Show, pure, unit, ($), discard , (>=) , (<=) ,(==),(&&) , not ,(+) , show , void)
import Screens.Types (ReferralScreenState, ReferralType(..))
import Components.BottomNavBar as BottomNavBar
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Controllers as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PopUpModal.Controller as PopUpModal
import PrestoDOM (Eval, continue, exit, continueWithCmd , updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress , trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Data.String (length)
import JBridge (hideKeyboardOnNavigation, toast, showDialer, firebaseLogEvent)
import Services.Config (getSupportNumber)
import Debug.Trace (spy)
import Helpers.Utils (clearTimer)
import Storage (setValueToLocalNativeStore, KeyStore(..))


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REFERRAL_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REFERRAL_SCREEN)
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen REFERRAL_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_button" "link_referral_code"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "primary_button" "no_action"
    PrimaryEditTextAction1 act -> case act of
      PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_text_changed"
      PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_text_field_click"
    PrimaryEditTextAction2 act -> case act of
      PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "confirm_referral_code_text_changed"
      PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "confirm_referral_code_text_field_click"
    GenericHeaderActionController act -> case act of 
      GenericHeader.PrefixImgOnClick -> do 
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    PasswordModalAction act -> case act of
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "confirm_password"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      PopUpModal.ETextController (PrimaryEditTextController.TextChanged valId newVal) -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_changed" "popup_modal_edit_password"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "close_icon"
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "no_action"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "countdown_updated" 
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "no_action"
    SuccessScreenExpireCountDwon seconds id status timerId -> do
      if status == "EXPIRED" then trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "countdown_expired"
        else trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "countdown_updated"
    ContactSupportAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "cancel"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "call_support"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "close_icon"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "countdown_updated" 
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "no_action"
    GoToAlertScreen -> do
      trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "for_updates_see_alerts"
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    EnableReferralFlow -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "enable_referral_flow"
    EnableReferralFlowNoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "enable_referral_flow_no_action"
    SuccessScreenRenderAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "your_referral_code_is_linked"

data Action = BottomNavBarAction BottomNavBar.Action
            | GenericHeaderActionController GenericHeader.Action
            | PrimaryEditTextAction1 PrimaryEditText.Action
            | PrimaryEditTextAction2 PrimaryEditText.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | PasswordModalAction PopUpModal.Action
            | SuccessScreenExpireCountDwon Int String String String 
            | ContactSupportAction PopUpModal.Action
            | GoToAlertScreen
            | EnableReferralFlow
            | BackPressed
            | EnableReferralFlowNoAction
            | AfterRender
            | SuccessScreenRenderAction

data ScreenOutput = GoToHomeScreen
                  | GoBack
                  | GoToRidesScreen
                  | GoToProfileScreen
                  | GoToNotifications
                  | LinkReferralApi ReferralScreenState
eval :: Action -> ReferralScreenState -> Eval Action ScreenOutput ReferralScreenState

eval BackPressed state = exit $ GoBack

eval EnableReferralFlow state = do
  if (state.props.enableReferralFlowCount >= 5 ) then do
    continue state {props {stage = ReferralFlow}}
    else do
      continue state {props {enableReferralFlowCount = state.props.enableReferralFlowCount + 1}}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = continue state { props = state.props { passwordPopUpVisible = not state.props.passwordPopUpVisible }}

eval GoToAlertScreen state = do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ GoToNotifications

eval (PrimaryEditTextAction1 (PrimaryEditText.TextChanged valId newVal)) state = do
  _ <- if length newVal >= 6 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit
  continue state {  props = state.props { primarybtnActive = ((length newVal) == 6 && (newVal == state.data.confirmReferralCode))}
                    , data = state.data { referralCode = if length newVal <= 6 then newVal else state.data.referralCode }} 

eval (PrimaryEditTextAction2 (PrimaryEditText.TextChanged valId newVal)) state = do
  _ <- if length newVal >= 6 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit
  continue state {  props = state.props { primarybtnActive = ((length newVal) == 6 && (newVal == state.data.referralCode))}
                   , data = state.data { confirmReferralCode = if length newVal <= 6 then newVal else state.data.confirmReferralCode }}


eval (PasswordModalAction (PopUpModal.OnImageClick)) state = do 
    _ <- pure $ hideKeyboardOnNavigation true 
    continue state { data = state.data{ password = "" }, props = state.props { passwordPopUpVisible = not state.props.passwordPopUpVisible , confirmBtnActive = false }}


eval (PasswordModalAction (PopUpModal.OnButton2Click)) state = do 
    _ <- pure $ hideKeyboardOnNavigation true 
    exit $ LinkReferralApi state 


eval (PasswordModalAction (PopUpModal.ETextController (PrimaryEditTextController.TextChanged valId newVal))) state = do
  _ <- if length newVal >= 5 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit
  continue state{ data{ password = newVal } , props { confirmBtnActive = (length newVal == 5)}}

eval (ContactSupportAction (PopUpModal.OnButton1Click)) state = continue state { props = state.props { callSupportPopUpVisible = not state.props.callSupportPopUpVisible  }}
eval (ContactSupportAction (PopUpModal.OnButton2Click)) state = do 
    void $ pure $  showDialer $ getSupportNumber ""
    continue state { props = state.props { callSupportPopUpVisible = not state.props.callSupportPopUpVisible  }}

eval (SuccessScreenExpireCountDwon seconds id status timerId) state = if status == "EXPIRED" then do  
  _ <- pure $ clearTimer timerId
  continue state{props {stage = QRScreen}} else continue state

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do 
  pure $ hideKeyboardOnNavigation true 
  case item of
    "Home" -> exit GoToHomeScreen
    "Rides" -> exit GoToRidesScreen
    "Profile" -> exit $ GoToProfileScreen
    "Alert" -> do
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ GoToNotifications
    _ -> continue state

eval _ state = continue state
