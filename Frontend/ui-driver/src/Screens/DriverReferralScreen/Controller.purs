module Screens.DriverReferralScreen.Controller where

import JBridge (minimizeApp, firebaseLogEvent, hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, shareImageMessage, setCleverTapUserProp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types 
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Components.GenericHeader as GenericHeader
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Prelude (bind, class Show, pure, unit, ($), discard, (>=), (<=), (==), (&&), not, (+), show, void, (<>), when, map, negate, (-), (>), (/=), (<))
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
import Foreign (unsafeToForeign)
import Data.Array (find)
import Services.API 

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverReferralScreen"
    BackPressed -> trackAppBackPress appId (getScreen REFERRAL_SCREEN)
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
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_button_action" "next_on_click"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_button_action" "no_action"
    ReferredDriversAPIResponseAction val -> pure unit
    ChangeTab tab -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "change_tab" (show tab)
    ShowReferedInfo referralInfoPopType -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "change_tab" (show referralInfoPopType)
    GoToLeaderBoard -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "change_tab" "leaderboard"
    UpdateDriverPerformance _ -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "referral_screen_response_action" "referral_screen_response_action"
    UpdateLeaderBoard _ -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "referral_screen_leaderboard_rank_action_action" "referral_screen_leaderboard_rank_action_action"

data Action = BackPressed
            | AfterRender
            | GenericHeaderActionController GenericHeader.Action
            | ShowQRCode
            | ShareOptions
            | BottomNavBarAction BottomNavBar.Action
            | LearnMore
            | PrimaryButtonActionController DriverReferralScreenState PrimaryButton.Action
            | ReferredDriversAPIResponseAction Int
            | ChangeTab DriverReferralType
            | ShowReferedInfo ReferralInfoPopType
            | GoToLeaderBoard
            | UpdateDriverPerformance GetPerformanceRes
            | UpdateLeaderBoard LeaderBoardRes

data ScreenOutput = GoToHomeScreen DriverReferralScreenState
                  | GoToNotifications DriverReferralScreenState
                  | SubscriptionScreen DriverReferralScreenState
                  | GoToDriverContestScreen DriverReferralScreenState
                  | EarningsScreen DriverReferralScreenState
                  | GoBack 

eval :: Action -> DriverReferralScreenState -> Eval Action ScreenOutput DriverReferralScreenState

eval BackPressed state = 
  if state.props.showDriverReferralQRCode then 
    continue state{props{showDriverReferralQRCode = false}}
  else if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then 
    continue state{props{referralInfoPopType = NO_REFERRAL_POPUP}}
  else exit $ GoBack

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = exit $ GoBack

eval ShowQRCode state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_app_qr_code_click"
  continue state {props {showDriverReferralQRCode = true}}

eval ShareOptions state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_share_referral_code_click"
  let message = "👋 Hey,\n\nMy " <> state.data.config.appData.name <> " Referral Code is " <> (state.data.referralCode) <> ".\n\nScan the QR code and download " <> state.data.config.appData.name <> " app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
  void $ pure $ shareImageMessage message (shareImageMessageConfig state)
  continue state

eval LearnMore state = exit $ GoToDriverContestScreen state

eval (PrimaryButtonActionController primaryButtonState (PrimaryButton.OnClick) ) state = continue state {props {showDriverReferralQRCode = false}}

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  pure $ hideKeyboardOnNavigation true
  case item of
    "Home" -> exit $ GoToHomeScreen state
    "Earnings" -> exit $ EarningsScreen state
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

eval (UpdateDriverPerformance (GetPerformanceRes resp)) state = do 
  continue state {data {totalReferredDrivers = fromMaybe 0 resp.referrals.totalReferredDrivers, totalActivatedCustomers = resp.referrals.totalActivatedCustomers, totalReferredCustomers = resp.referrals.totalReferredCustomers}}

eval (UpdateLeaderBoard (LeaderBoardRes resp)) state = do
  let currentDriverRank = case find (\(DriversInfo driverInfo) -> driverInfo.isCurrentDriver && driverInfo.totalRides /= 0) resp.driverList of
        Just (DriversInfo currentDriver) -> Just currentDriver.rank
        _ -> Nothing
  continue state {data {totalEligibleDrivers = resp.totalEligibleDrivers, rank = currentDriverRank}}

eval (ChangeTab tab) state = 
  continue state {props {driverReferralType = tab}}

eval (ShowReferedInfo referralInfoPopType) state = 
  continue state {props {referralInfoPopType = referralInfoPopType}}

eval GoToLeaderBoard state = exit $ GoToDriverContestScreen state

eval _ state = continue state

shareImageMessageConfig :: DriverReferralScreenState -> ShareImageConfig
shareImageMessageConfig state = {
  code : state.data.referralCode,
  viewId : getNewIDWithTag "DriverReferralQRScreen",
  logoId : getNewIDWithTag "DriverReferralScreenLogo",
  isReferral : true
  }