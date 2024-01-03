module Screens.DriverReferralScreen.Controller where

import JBridge (minimizeApp, firebaseLogEvent, hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, shareImageMessage)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (DriverReferralScreenState)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Components.GenericHeader as GenericHeader
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Prelude (bind, class Show, pure, unit, ($), discard, (>=), (<=), (==), (&&), not, (+), show, void, (<>), when, map, (-), (>), (/=), (<))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Components.BottomNavBar as BottomNavBar
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Helpers.Utils (incrementValueOfLocalStoreKey)
import Components.PrimaryButton as PrimaryButton
import Common.Types.App (ShareImageConfig)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverReferralScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    ShowQRCode -> pure unit
    ShareOptions -> pure unit
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen REFERRAL_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    LearnMore -> pure unit
    PrimaryButtonActionController state act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button_action" "next_on_click"
        trackAppEndScreen appId (getScreen CHOOSE_LANGUAGE_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button_action" "no_action"
    ReferredDriversAPIResponseAction val -> pure unit

data Action = BackPressed
            | AfterRender
            | GenericHeaderActionController GenericHeader.Action
            | ShowQRCode
            | ShareOptions
            | BottomNavBarAction BottomNavBar.Action
            | LearnMore
            | PrimaryButtonActionController DriverReferralScreenState PrimaryButton.Action
            | ReferredDriversAPIResponseAction Int


data ScreenOutput = GoToHomeScreen DriverReferralScreenState
                  | GoToRidesScreen DriverReferralScreenState
                  | GoToNotifications DriverReferralScreenState
                  | SubscriptionScreen DriverReferralScreenState
                  | GoToDriverContestScreen DriverReferralScreenState
                  | GoBack


eval :: Action -> DriverReferralScreenState -> Eval Action ScreenOutput DriverReferralScreenState

eval BackPressed state = 
  if state.props.showDriverReferralQRCode then 
    continue state{props{showDriverReferralQRCode = false}}
  else exit $ GoBack

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = exit $ GoBack

eval ShowQRCode state = continue state {props {showDriverReferralQRCode = true}}

eval ShareOptions state = do
  let message = "👋 Hey,\n\nMy " <> state.data.config.appData.name <> " Referral Code is " <> (state.data.referralCode) <> ".\n\nScan the QR code and download " <> state.data.config.appData.name <> " app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
  void $ pure $ shareImageMessage message (shareImageMessageConfig state)
  continue state

eval LearnMore state = exit $ GoToDriverContestScreen state

eval (PrimaryButtonActionController primaryButtonState (PrimaryButton.OnClick) ) state = continue state {props {showDriverReferralQRCode = false}}

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  pure $ hideKeyboardOnNavigation true
  case item of
    "Home" -> exit $ GoToHomeScreen state
    "Rides" -> exit $ GoToRidesScreen state
    "Alert" -> do
      void $ pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_alert_click"
      exit $ GoToNotifications state
    "Join" -> do
      let driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_SUBSCRIPTION
      void $ pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      void $ pure $ metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let _ = unsafePerformEffect $ firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (ReferredDriversAPIResponseAction val) state = continue state {data {referredDrivers = show val}, props {showNewDriverReferralText = val < 1}}

eval _ state = continue state

shareImageMessageConfig :: DriverReferralScreenState -> ShareImageConfig
shareImageMessageConfig state = {
  code : state.data.referralCode,
  viewId : getNewIDWithTag "DriverReferralQRScreen",
  logoId : getNewIDWithTag "DriverReferralScreenLogo",
  isReferral : true
  }