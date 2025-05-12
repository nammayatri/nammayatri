module Screens.Benefits.BenefitsScreen.Controller where

import JBridge (shareTextMessage, minimizeApp, firebaseLogEvent, hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, shareImageMessage, openUrlInApp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($))
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
import Helpers.Utils (incrementValueOfLocalStoreKey, generateReferralLink, generateQR, contactSupportNumber, getCityConfig)
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
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.OptionsMenu as OptionsMenu
import Components.PopUpModal as PopUpModal
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST

instance showAction :: Show Action where
  show _ = ""
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
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action
            | OptionsMenuAction OptionsMenu.Action
            | PopUpModalLogoutAction PopUpModal.Action
            | BottomDrawerListAC BottomDrawerList.Action
            | WhatsAppClick
            | ContinueButtonAction PrimaryButton.Action

data ScreenOutput = GoToHomeScreen BenefitsScreenState
                  | GoToNotifications BenefitsScreenState
                  | SubscriptionScreen BenefitsScreenState
                  | GoToDriverContestScreen BenefitsScreenState
                  | EarningsScreen BenefitsScreenState
                  | GoBack
                  | GoToLmsVideoScreen BenefitsScreenState
                  | GoToCustomerReferralTrackerScreen Boolean BenefitsScreenState
                  | GoToRegistrationScreen BenefitsScreenState
                  | SelectLang BenefitsScreenState
                  | LogoutAccount
                  | GoToFaqsScreen BenefitsScreenState

eval :: Action -> BenefitsScreenState -> Eval Action ScreenOutput BenefitsScreenState

eval BackPressed state = 
  if state.props.showDriverReferralQRCode then 
    continue state{props{showDriverReferralQRCode = false}}
  else if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then 
    continue state{props{referralInfoPopType = NO_REFERRAL_POPUP}}
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.HIDE}}
  else if state.props.menuOptions then continue state { props { menuOptions = false}}
  else if state.props.fromRegistrationScreen then exit $ GoToRegistrationScreen state
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

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props{menuOptions = not state.props.menuOptions}}

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.PrefixImgOnClick)) state = continueWithCmd state [pure BackPressed]

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState { props { logoutModalView = true }}
    "contact_support" -> continue newState { props { contactSupportModal = ST.SHOW}}
    "change_language" -> exit $ SelectLang newState
    "faqs" -> exit $ GoToFaqsScreen newState
    _ -> continue newState
  
eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView = false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (BottomDrawerListAC BottomDrawerList.Dismiss) state = continue state { props { contactSupportModal = ST.HIDE}}

eval (BottomDrawerListAC BottomDrawerList.OnAnimationEnd) state = continue state { props { contactSupportModal = if state.props.contactSupportModal == ST.ANIMATING then ST.HIDE else state.props.contactSupportModal}}

eval (BottomDrawerListAC (BottomDrawerList.OnItemClick item)) state = do
  case item.identifier of
    "whatsapp" -> continueWithCmd state [pure WhatsAppClick]
    "call" -> do
                void $ pure $ unsafePerformEffect $ contactSupportNumber ""
                continue state
    _ -> continue state

eval WhatsAppClick state = continueWithCmd state [do
  let cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      supportPhone = cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> getValueToLocalStore MOBILE_NUMBER_KEY
      dlNumber = getValueToLocalStore ENTERED_DL
      rcNumber = getValueToLocalStore ENTERED_RC
      dl = if (dlNumber /= "__failed") then ("%0ADL%20Number%3A%20"<> dlNumber) else ""
      rc = if (rcNumber /= "__failed") then ("%0ARC%20Number%3A%20"<> rcNumber) else ""
  void $ JB.openUrlInApp $ "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  pure NoAction
  ]

eval (ContinueButtonAction (PrimaryButton.OnClick)) state = do
  void $ pure $ setValueToLocalNativeStore TRAININGS_COMPLETED_STATUS "true"
  exit $ GoToRegistrationScreen state

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