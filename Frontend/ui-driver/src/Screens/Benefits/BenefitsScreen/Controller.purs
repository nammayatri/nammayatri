module Screens.Benefits.BenefitsScreen.Controller where

import JBridge (shareTextMessage, minimizeApp, firebaseLogEvent, hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, shareImageMessage, openUrlInApp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($), show, (<>))
import PrestoDOM (Eval, update, continue, exit)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Screens (getScreen, ScreenName(..))
import Screens.Types 
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent, logEventWithMultipleParams)
import Components.GenericHeader as GenericHeader
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Prelude (bind, class Show, pure, unit, ($), discard, (>=), (<=), (==), (&&), not, (+), show, void, (<>), when, map, negate, (-), (>), (/=), (<))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.BottomNavBar as BottomNavBar
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Helpers.Utils (incrementValueOfLocalStoreKey, generateReferralLink, generateQR)
import Components.PrimaryButton as PrimaryButton
import Common.Types.App (ShareImageConfig, LazyCheck(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import Foreign (unsafeToForeign)
import Data.Array (find)
import Services.API
import Effect.Uncurried (runEffectFn4)
import JBridge as JB
import Screens.Benefits.BenefitsScreen.Transformer (buildLmsModuleRes)
import PrestoDOM.List (ListItem)
import Storage (getValueToLocalStore, KeyStore(..))
import Components.BannerCarousel as BannerCarousel
import Data.String as DS
import Locale.Utils(getLanguageLocale, languageKey)
import RemoteConfig as RC
import SessionCache (getValueFromWindow)
import Data.Array as DA
import Data.Int as DI
import PrestoDOM.Core (processEvent)
import Data.Function.Uncurried (runFn2)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU

instance showAction :: Show Action where
  show (BackPressed) = "BackPressed"
  show (AfterRender) = "AfterRender"
  show (GenericHeaderActionController var1) = "GenericHeaderActionController_" <> show var1
  show (ShowQRCode) = "ShowQRCode"
  show (ShareQRLink) = "ShareQRLink"
  show (BottomNavBarAction var1) = "BottomNavBarAction_" <> show var1
  show (LearnMore) = "LearnMore"
  show (PrimaryButtonActionController _ var1) = "PrimaryButtonActionController_" <>  show var1
  show (ReferredDriversAPIResponseAction _) = "ReferredDriversAPIResponseAction"
  show (ChangeTab var1) = "ChangeTab_" <> show var1
  show (ShowReferedInfo var1) = "ShowReferedInfo_" <> show var1
  show (GoToLeaderBoard) = "GoToLeaderBoard"
  show (UpdateDriverPerformance var1) = "UpdateDriverPerformance_" <> show var1
  show (UpdateLeaderBoard var1) = "UpdateLeaderBoard_" <> show var1
  show (RenderQRCode) = "RenderQRCode"
  show (OpenModule var1) = "OpenModule_" <> show var1
  show (UpdateModuleList var1) = "UpdateModuleList_" <> show var1
  show (UpdateModuleListErrorOccurred) = "UpdateModuleListErrorOccurred"
  show (GoToCustomerReferralTracker _) = "GoToCustomerReferralTracker"
  show (SetBannerItem _) = "SetBannerItem"
  show (BannerCarousal var1) = "BannerCarousal_" <> show var1
  show (UpdateBanner) = "UpdateBanner"
  show (BannerChanged _) = "BannerChanged"
  show (BannerStateChanged _) = "BannerStateChanged"
  show (NoAction) = "NoAction"
  show (GullakSDKResponse _) = "GullakSDKResponse"
  show (GullakBannerClick) = "GullakBannerClick"
  show (UpdateReferralCode _) = "UpdateReferralCode"
  show (GoToClaimReward) = "GoToClaimReward"
  show (YoutubeVideoStatus _) = "YoutubeVideoStatus"
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BenefitsScreen"
    BackPressed -> trackAppBackPress appId (getScreen REFERRAL_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    ShowQRCode -> pure unit
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
    RenderQRCode -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "screen" "render_qr_code"
    OpenModule _->  trackAppActionClick appId (getScreen REFERRAL_SCREEN) "go_to_module" "go_to_lms_video_screen"
    UpdateModuleList _ ->  trackAppActionClick appId (getScreen REFERRAL_SCREEN) "update_module_list" "update_module_list"
    UpdateModuleListErrorOccurred -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "update_module_list_error_occurred" "update_module_list"
    ShareQRLink -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "screen" "render_qr_link"
    GoToCustomerReferralTracker openPP -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "screen" "go_to_customer_referral_tracker"
    _ -> defaultPerformLog action appId

data Action = BackPressed
            | AfterRender
            | GenericHeaderActionController GenericHeader.Action
            | ShowQRCode
            | ShareQRLink
            | BottomNavBarAction BottomNavBar.Action
            | LearnMore
            | PrimaryButtonActionController BenefitsScreenState PrimaryButton.Action
            | ReferredDriversAPIResponseAction Int
            | ChangeTab DriverReferralType
            | ShowReferedInfo ReferralInfoPopType
            | GoToLeaderBoard
            | UpdateDriverPerformance GetPerformanceRes
            | UpdateLeaderBoard LeaderBoardRes
            | RenderQRCode
            | OpenModule LmsModuleRes
            | UpdateModuleList LmsGetModuleRes
            | UpdateModuleListErrorOccurred
            | GoToCustomerReferralTracker Boolean
            | SetBannerItem ListItem
            | BannerCarousal BannerCarousel.Action
            | UpdateBanner
            | BannerChanged String
            | BannerStateChanged String
            | NoAction
            | GullakSDKResponse String
            | GullakBannerClick
            | UpdateReferralCode GenerateReferralCodeRes
            | GoToClaimReward
            | YoutubeVideoStatus String
data ScreenOutput = GoToHomeScreen BenefitsScreenState
                  | GoToNotifications BenefitsScreenState
                  | SubscriptionScreen BenefitsScreenState
                  | GoToDriverContestScreen BenefitsScreenState
                  | EarningsScreen BenefitsScreenState
                  | GoBack
                  | GoToLmsVideoScreen BenefitsScreenState
                  | GoToCustomerReferralTrackerScreen Boolean BenefitsScreenState
                  | GoToDriverClaimRewardScreen BenefitsScreenState

eval :: Action -> BenefitsScreenState -> Eval Action ScreenOutput BenefitsScreenState

eval BackPressed state = 
  if state.props.showDriverReferralQRCode then 
    continue state{props{showDriverReferralQRCode = false}}
  else if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then 
    continue state{props{referralInfoPopType = NO_REFERRAL_POPUP}}
  else exit $ GoToHomeScreen state

eval (GoToCustomerReferralTracker openPP) state = exit $ GoToCustomerReferralTrackerScreen openPP state

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = exit $ GoBack

eval (GullakSDKResponse _ ) state = 
  continueWithCmd state { props { glBannerClickable = true}}
    [do
      void $ EHU.terminateLoader ""
      pure NoAction
    ]

eval ShowQRCode state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_app_qr_code_click"
  continue state {props {showDriverReferralQRCode = true}}

eval ShareQRLink state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_contest_share_referral_code_click"
  let title = getString $ SHARE_NAMMA_YATRI "SHARE_NAMMA_YATRI"
  let message = (getString SHARE_NAMMA_YATRI_MESSAGE) <> title <> " " <> (getString NOW) <> "! \n" <> (generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode state.props.driverReferralType) <> (getString BE_OPEN_CHOOSE_OPEN) 
  _ <- pure $ shareTextMessage title message
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
    "Earnings" -> exit $ EarningsScreen state
    _ -> continue state

eval (UpdateDriverPerformance (GetPerformanceRes resp)) state = do 
  continue state {
              data{
                totalReferredDrivers = fromMaybe 0 resp.referrals.totalReferredDrivers, 
                totalActivatedCustomers = resp.referrals.totalActivatedCustomers, 
                totalReferredCustomers = resp.referrals.totalReferredCustomers,
                eligiblePayoutAmount = resp.referrals.eligiblePayoutAmount,
                lastPayoutAt = resp.referrals.lastPayoutAt,
                payoutAmountPaid = resp.referrals.payoutAmountPaid,
                payoutVpa = resp.referrals.payoutVpa
              },
              props{
                isPayoutEnabled = Just resp.referrals.isPayoutEnabled
              }
            }
eval (UpdateReferralCode (GenerateReferralCodeRes resp)) state = do
  continue state {data {referralCode = resp.referralCode}}

eval (UpdateLeaderBoard (LeaderBoardRes resp)) state = do
  let currentDriverRank = case find (\(DriversInfo driverInfo) -> driverInfo.isCurrentDriver && driverInfo.totalRides /= 0) resp.driverList of
        Just (DriversInfo currentDriver) -> Just currentDriver.rank
        _ -> Nothing
  continue state {data {totalEligibleDrivers = resp.totalEligibleDrivers, rank = currentDriverRank}}

eval (ChangeTab tab) state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField "ny_driver_referral_scn_changetab" $ [{key : "Tab", value : unsafeToForeign (show tab)}]
  continueWithCmd state {props {driverReferralType = tab}}
    [ do
      runEffectFn4 generateQR (generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode tab) (getNewIDWithTag "ReferralQRCode") 500 0
      pure $ RenderQRCode
    ]

eval (ShowReferedInfo referralInfoPopType) state = 
  continue state {props {referralInfoPopType = referralInfoPopType}}

eval GoToLeaderBoard state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_go_to_leaderboard"
  exit $ GoToDriverContestScreen state

eval (OpenModule selectedModule) state = updateAndExit state { props{selectedModule = Just selectedModule}} $ GoToLmsVideoScreen state { props{selectedModule = Just selectedModule}}

eval (UpdateModuleList modules) state = continue state {data {moduleList = buildLmsModuleRes modules}, props {showShimmer = false}}

eval UpdateModuleListErrorOccurred state = continue state {props {showShimmer = false}}

eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}, props{bannerLength = DA.length $ getRemoteBannerConfigs BannerCarousal}}

eval UpdateBanner state = do
  if state.data.bannerData.bannerScrollState == "1" then continue state
  else do
    let nextBanner = state.data.bannerData.currentBanner + 1
        bannerArray = getRemoteBannerConfigs BannerCarousal
        updatedIdx = if nextBanner >= (DA.length bannerArray) then 0 else nextBanner
        newState = state{data {bannerData{currentBanner = updatedIdx, currentPage = updatedIdx}}}
    continue newState

eval (BannerChanged item) state = do
  let currentBanner = DI.fromString item
  case currentBanner of
    Just idx -> do 
        let newState = state{data {bannerData{currentBanner = idx}}}
        if state.data.bannerData.currentPage /= idx then void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit 
          else pure unit
        continue newState
    Nothing  -> continue state

eval (BannerStateChanged item) state = do
  let newState = state{data {bannerData{bannerScrollState = item}}}
  continue newState

eval (BannerCarousal (BannerCarousel.OnClick index)) state =
  continueWithCmd state [do
    let banners = getRemoteBannerConfigs BannerCarousal
    case DA.index banners index of
      Just config -> do
        let _ = runFn2 EHC.updatePushInIdMap "referralBannerCarousel" false
        case config.type of
          BannerCarousel.Remote link -> do
            void $ openUrlInApp link
            pure NoAction
          _ -> pure NoAction
      Nothing -> pure NoAction
  ] 

eval GullakBannerClick state = continue state { props { glBannerClickable = false}}

eval GoToClaimReward state = exit $ GoToDriverClaimRewardScreen state
  
eval _ state = update state

shareImageMessageConfig :: BenefitsScreenState -> ShareImageConfig
shareImageMessageConfig state = {
  code : state.data.referralCode,
  viewId : getNewIDWithTag "BenefitsQRScreen",
  logoId : getNewIDWithTag "BenefitsScreenLogo",
  isReferral : true
  }

getRemoteBannerConfigs :: forall action. (BannerCarousel.Action -> action) -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
getRemoteBannerConfigs action = do
  let location = DS.toLower $ getValueToLocalStore DRIVER_LOCATION
      language = getLanguage $ getLanguageLocale languageKey
      variant = getValueToLocalStore VEHICLE_VARIANT
      configName = "driver_referral_banner" <> language
      datas = (RC.carouselConfigData location configName "driver_referral_banner_en" (getValueFromWindow "DRIVER_ID") "" variant)
  BannerCarousel.remoteConfigTransformer datas action Nothing
  where
    getLanguage :: String -> String
    getLanguage lang = 
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"