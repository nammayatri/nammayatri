module Screens.DriverReferralScreen.Controller where

import JBridge (minimizeApp, firebaseLogEvent, hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, shareImageMessage, setCleverTapUserProp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (DriverReferralScreenState, ReferredUserType(..), UserReferralType(..))
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Components.GenericHeader as GenericHeader
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Prelude (bind, class Show, pure, unit, ($), discard, (>=), (<=), (==), (&&), not, (+), show, void, (<>), when, map, (-), (>), (/=), (<))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Components.BottomNavBar as BottomNavBar
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Helpers.Utils (incrementValueOfLocalStoreKey, generateQR)
import Components.PrimaryButton as PrimaryButton
import Common.Types.App (ShareImageConfig)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import Foreign (unsafeToForeign)
import Services.API (GetPerformanceRes(..))
import Effect.Uncurried(runEffectFn4)
import JBridge (openUrlInApp)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverReferralScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen DRIVER_REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    ShowQRCode -> pure unit
    ShareOptions val-> pure unit
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen DRIVER_REFERRAL_SCREEN)
    LearnMore -> pure unit
    PrimaryButtonActionController state act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "primary_button_action" "next_on_click"
        trackAppEndScreen appId (getScreen DRIVER_REFERRAL_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "primary_button_action" "no_action"
    ReferredDriversAPIResponseAction val -> pure unit
    ReferredInfo val -> pure unit
    ReferralStageChange val -> pure unit
    UpdateDriverPerformance _ -> trackAppScreenEvent appId (getScreen DRIVER_REFERRAL_SCREEN) "in_screen" "update_driver_performance"
    NoAction -> trackAppScreenEvent appId (getScreen DRIVER_REFERRAL_SCREEN) "in_screen" "no_action"
    ReferralQrRendered _  _ -> trackAppScreenEvent appId (getScreen DRIVER_REFERRAL_SCREEN) "in_screen" "referral_qr_rendered"
    KnowMore -> trackAppActionClick appId (getScreen DRIVER_REFERRAL_SCREEN) "in_screen" "know_more_click"
    
data Action = BackPressed
            | AfterRender
            | GenericHeaderActionController GenericHeader.Action
            | ShowQRCode
            | ShareOptions String
            | BottomNavBarAction BottomNavBar.Action
            | LearnMore
            | PrimaryButtonActionController DriverReferralScreenState PrimaryButton.Action
            | ReferredDriversAPIResponseAction Int
            | ReferredInfo ReferredUserType
            | ReferralStageChange UserReferralType
            | UpdateDriverPerformance GetPerformanceRes
            | ReferralQrRendered String String
            | NoAction
            | KnowMore


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
  else if state.props.referralInfo /= NO_POPUP then
    continue state {props{referralInfo = NO_POPUP}}
  else exit $ GoBack

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = exit $ GoBack

eval ShowQRCode state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_app_qr_code_click"
  continue state {props {showDriverReferralQRCode = true}}

eval (ShareOptions val) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_share_referral_code_click"
  let message = "👋 Hey,\n\nMy " <> state.data.config.appData.name <> " Referral Code is " <> (state.data.referralCode) <> ".\n\nScan the QR code and download " <> state.data.config.appData.name <> " app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
  void $ pure $ shareImageMessage message (shareImageMessageConfig val state)
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

eval (ReferredDriversAPIResponseAction val) state = do
  void $ pure $ setCleverTapUserProp [{key : "Referral Count", value : unsafeToForeign val}]
  continue state {data {referredDrivers = show val}, props {showNewDriverReferralText = val < 1}}

eval (ReferredInfo val) state = continue state {props {referralInfo = val }}

eval (ReferralStageChange val) state = do
  if (Just val == state.props.currentReferralItem) then do
    continue state {props{currentReferralItem = Nothing}}
  else do
    continue state {props{currentReferralItem = Just val}}
  
eval (UpdateDriverPerformance (GetPerformanceRes performanceRes)) state = continue state {data {referredCustomers = show performanceRes.referrals.totalReferredCustomers, activatedCustomers = show performanceRes.referrals.totalActivatedCustomers}}

eval (ReferralQrRendered id link) state = continueWithCmd state [ do
            runEffectFn4 generateQR link id 200 0
            pure $ NoAction
          ]

eval (KnowMore) state = continueWithCmd state[ do
      _ <- openUrlInApp state.data.cityConfig.referralConfig.referralVideoLink
      pure $ NoAction
  ]

eval _ state = continue state

shareImageMessageConfig :: String -> DriverReferralScreenState -> ShareImageConfig
shareImageMessageConfig val state = {
  code : state.data.referralCode,
  viewId : getNewIDWithTag val,
  logoId : getNewIDWithTag "DriverReferralScreenLogo",
  isReferral : true
  }