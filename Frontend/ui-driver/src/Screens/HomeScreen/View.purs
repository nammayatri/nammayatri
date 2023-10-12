{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.View where

import Screens.HomeScreen.ComponentConfig

import Animation (scaleYAnimWithDuration)
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..), APIPaymentStatus(..))
import Components.Banner.Controller as BannerConfig
import Components.Banner.View as Banner
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.ChatView as ChatView
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PopUpModal as PopUpModal
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.RideCompletedCard as RideCompletedCard
import Components.SelectListModal as SelectListModal
import Components.StatsModel as StatsModel
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Int (ceil, toNumber, fromString)
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Engineering.Helpers.Commons (flowRunner, getCurrentUTC)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Utils (getValueFromConfig)
import MerchantConfig.Utils as MU
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (<<<), (<>), (==), (>), (>=), (||), (<=), show, void, (/=), when, (+))
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textSize, textView, visibility, weight, width, topShift  )
import PrestoDOM (BottomSheetState(..), alignParentBottom, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, peakHeight, stroke, text, textSize, textView, visibility, weight, width, imageWithFallback, adjustViewWithKeyboard, lottieAnimationView, relativeLayout, ellipsize, singleLine)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (coordinatorLayout)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens as ScreenNames
import Screens.HomeScreen.Controller (Action(..), RideRequestPollingData, ScreenOutput, ScreenOutput(GoToHelpAndSupportScreen), checkPermissionAndUpdateDriverMarker, eval)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.Types (DisabilityType(..), DriverStatus(..), DriverStatusResult(..), HomeScreenStage(..), HomeScreenState, KeyboardModalType(..), PillButtonState(..), SubscriptionBannerType(..), TimerStatus(..), LocalStoreSubscriptionInfo)
import Screens.Types as ST
import Services.API (GetRidesHistoryResp(..), OrderStatusRes(..))
import Services.API (Status(..))
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore, getValueToLocalNativeStore, isLocalStageOn, setValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Constants (defaultDensity)

screen :: HomeScreenState -> Screen Action HomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "HomeScreen"
  , globalEvents : [
        ( \push -> do
          _ <- pure $ printLog "initial State" initialState
          _ <- HU.storeCallBackForNotification push Notification
          _ <- HU.storeCallBackTime push TimeUpdate
          _ <- runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
          when (getValueToLocalNativeStore IS_RIDE_ACTIVE == "true" && initialState.data.activeRide.status == NOTHING) do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
              (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "1" "0" "true" "null" "null"
              case (activeRideResponse.list DA.!! 0) of
                Just ride -> lift $ lift $ doAff do liftEffect $ push $ RideActiveAction ride
                Nothing -> do
                           setValueToLocalStore IS_RIDE_ACTIVE "false"
                           void $ pure $ JB.setCleverTapUserProp "Driver On-ride" "No"
          let startingTime = (HU.differenceBetweenTwoUTC (HU.getCurrentUTC "") (getValueToLocalStore SET_WAITING_TIME))
          if ((getValueToLocalStore IS_WAIT_TIMER_STOP) == "Triggered") && initialState.props.timerRefresh  then do
            _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP (show (PostTriggered))
            _ <- JB.waitingCountdownTimer startingTime push WaitTimerCallback
            pure unit
          else pure unit
          case (getValueToLocalNativeStore LOCAL_STAGE) of
            "RideRequested"  -> do
                                if (getValueToLocalStore RIDE_STATUS_POLLING) == "False" then do
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "True"
                                  let secondsOver = if EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true >= (rideRequestPollingData.duration) then (rideRequestPollingData.duration) else EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true
                                      counts = ceil $ (toNumber (rideRequestPollingData.duration - secondsOver) * 1000.0)/rideRequestPollingData.delay
                                  if counts > 0 then do
                                      void $ launchAff $ EHC.flowRunner defaultGlobalState $ rideRequestPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) counts rideRequestPollingData.delay initialState push Notification
                                  else
                                      void $ pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
                                  pure unit
                                  else pure unit
            "RideAccepted"   -> do
                                if (initialState.data.peekHeight == 0) then void $ push $ RideActionModalAction (RideActionModal.NoAction) else pure unit
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "2000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "5.0"
                                if (not initialState.props.chatcallbackInitiated) then do
                                  _ <- JB.storeCallBackMessageUpdated push initialState.data.activeRide.id "Driver" UpdateMessages
                                  _ <- JB.storeCallBackOpenChatScreen push OpenChatScreen
                                  _ <- JB.startChatListenerService
                                  _ <- pure $ JB.scrollOnResume push ScrollToBottom
                                  push InitializeChat
                                  pure unit
                                else pure unit
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  pure $ JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
                                  pure unit
                                  else pure unit
                                if (getValueToLocalStore RIDE_STATUS_POLLING) == "False" then do
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "True"
                                  _ <- launchAff $ EHC.flowRunner defaultGlobalState $ rideStatusPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) 20000.0 initialState push Notification
                                  pure unit
                                  else pure unit
            "RideStarted"    -> do
                                _ <- pure $ setValueToLocalNativeStore RIDE_START_LAT (HU.toString initialState.data.activeRide.src_lat)
                                _ <- pure $ setValueToLocalNativeStore RIDE_START_LON (HU.toString initialState.data.activeRide.src_lon)
                                _ <- pure $ setValueToLocalNativeStore RIDE_END_LAT (HU.toString initialState.data.activeRide.dest_lat)
                                _ <- pure $ setValueToLocalNativeStore RIDE_END_LON (HU.toString initialState.data.activeRide.dest_lon)
                                _ <- pure $ setValueToLocalNativeStore WAYPOINT_DEVIATION_COUNT "0"
                                _ <- pure $ setValueToLocalNativeStore TOLERANCE_EARTH "100.0"
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "50000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "25.0"
                                _ <- push RemoveChat
                                _ <- launchAff $ flowRunner defaultGlobalState $ launchMaps push TriggerMaps
                                if (initialState.data.peekHeight == 0) then void $ push $ RideActionModalAction (RideActionModal.NoAction) else pure unit
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  pure $ JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
                                  pure unit
                                  else pure unit
            "ChatWithCustomer" -> do
                                if (initialState.data.activeRide.notifiedCustomer && (not initialState.props.updatedArrivalInChat)) then do
                                  _ <- pure $ JB.sendMessage $ getMessageFromKey "dis1AP" "EN_US"
                                  push UpdateInChat
                                  else pure unit
            _                -> do
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "50000"
                                _ <- pure $ JB.removeAllPolylines ""
                                _ <- JB.reallocateMapFragment (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap")
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "25.0"
                                _ <- pure $ setValueToLocalStore SESSION_ID (JB.generateSessionId unit)
                                _ <- checkPermissionAndUpdateDriverMarker initialState
                                _ <- launchAff $ EHC.flowRunner defaultGlobalState $ checkCurrentRide push Notification
                                _ <- launchAff $ EHC.flowRunner defaultGlobalState $ paymentStatusPooling initialState.data.paymentState.invoiceId 4 5000.0 initialState push PaymentStatusAction
                                pure unit
          runEffectFn1 HU.consumeBP unit
          pure $ pure unit
        )
  ]
  , eval : (\state  action -> do
      let _ = spy "HomeScreen state -----" state
      let _ = spy "HomeScreen--------action" action
      eval state action)
  }


view :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]([ relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.white900
      , weight 1.0
      , afterRender
        (\action -> do
          _ <- push action
          _ <- JB.setFCMToken push $ SetToken
          _ <- JB.showMap (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap") (enableCurrentLocation state) "satellite" (17.0) push ShowMap
          _ <- JB.getCurrentPosition push CurrentLocation
          pure unit
        ) (const AfterRender)
      , onBackPressed push (const BackPressed)
      ][ Anim.screenAnimationFadeInOut $
          driverMapsHeaderView push state
        , rideActionModelView push state
        ]
      -- , if (getValueToLocalNativeStore PROFILE_DEMO) /= "false" then profileDemoView state push else linearLayout[][]       Disabled ProfileDemoView
      , if state.data.paymentState.makePaymentModal && (not $ DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer, RideCompleted]) then makePaymentModal push state else dummyTextView
      , if state.props.currentStage == RideCompleted then RideCompletedCard.view (getRideCompletedConfig state) (push <<< RideCompletedAC) else dummyTextView
      , if state.props.goOfflineModal then goOfflineModal push state else dummyTextView
      , if state.props.enterOtpModal then enterOtpModal push state else dummyTextView
      , if state.props.endRidePopUp then endRidePopView push state else dummyTextView
      , if state.props.cancelConfirmationPopup then cancelConfirmation push state else dummyTextView
      , if state.props.cancelRideModalShow then cancelRidePopUpView push state else dummyTextView
      , if state.props.currentStage == ChatWithCustomer then chatView push state else dummyTextView
      , if state.props.showBonusInfo then requestInfoCardView push state else dummyTextView
      , if state.props.silentPopUpView then popupModelSilentAsk push state else dummyTextView
      , if state.data.activeRide.waitTimeInfo then waitTimeInfoPopUp push state else dummyTextView
      , if state.props.showAccessbilityPopup then accessibilityPopUpView push state else dummyTextView
      , if state.data.paymentState.showRateCard then rateCardView push state else dummyTextView
      , if (state.props.showlinkAadhaarPopup && state.props.showAadharPopUp) then linkAadhaarPopup push state else dummyTextView
      , if state.props.rcDeactivePopup then PopUpModal.view (push <<< RCDeactivatedAC) (driverRCPopUpConfig state) else dummyTextView
      , if state.props.showRideRating then RatingCard.view (push <<< RatingCardAC) (getRatingCardConfig state) else dummyTextView
      , if (state.props.subscriptionPopupType == ST.FREE_TRIAL_POPUP) && (MU.getMerchant FunctionCall) == MU.NAMMAYATRI
           then PopUpModal.view (push <<< FreeTrialEndingAC) (freeTrialEndingPopupConfig state) 
           else linearLayout[visibility GONE][]
      , case HU.getPopupObjectFromSharedPrefs SHOW_JOIN_NAMMAYATRI of
          Just configObject -> if (isLocalStageOn HomeScreen) then PopUpModal.view (push <<< OfferPopupAC) (offerPopupConfig true configObject) else linearLayout[visibility GONE][]
          Nothing -> linearLayout[visibility GONE][]
      , if state.props.showOffer && (MU.getMerchant FunctionCall) == MU.NAMMAYATRI then PopUpModal.view (push <<< OfferPopupAC) (offerPopupConfig false (offerConfigParams state)) else dummyTextView
      , if (DA.any (_ == state.props.subscriptionPopupType)[ST.SOFT_NUDGE_POPUP,  ST.LOW_DUES_CLEAR_POPUP, ST.GO_ONLINE_BLOCKER] && (MU.getMerchant FunctionCall) == MU.NAMMAYATRI )
          then PopUpModal.view (push <<< PaymentPendingPopupAC) (paymentPendingPopupConfig state) 
        else linearLayout[visibility GONE][]
      , if state.props.showGenericAccessibilityPopUp then genericAccessibilityPopUpView push state else dummyTextView
  ] <> if (state.props.showChatBlockerPopUp || state.data.paymentState.showBlockingPopup )then [blockerPopUpView push state] else [])


blockerPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
blockerPopUpView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view 
    (push <<< if state.data.paymentState.showBlockingPopup then StartEarningPopupAC else PopUpModalChatBlockerAction ) 
    (if state.data.paymentState.showBlockingPopup then subsBlockerPopUpConfig state else chatBlockerPopUpConfig state)]
  
accessibilityPopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
accessibilityPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalAccessibilityAction) (accessibilityPopUpConfig state)]

driverMapsHeaderView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverMapsHeaderView push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL
      , background Color.white900
      ,cornerRadius 50.0
      ][ linearLayout
          [ width MATCH_PARENT
          , height $ V 2
          , background Color.greyTextColor
          , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then GONE else VISIBLE
          , alpha 0.1
          ][]
        , frameLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          ][  googleMap state
            , if (state.props.driverStatusSet == Offline && not state.data.paymentState.blockedDueToPayment) then offlineView push state else dummyTextView
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , PP.cornerRadii $ PTD.Corners 24.0  false false true true
                  , background $ Color.white900
                  , stroke $ (if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then "0," else "1,") <> "#E5E7EB"
                  ][  driverDetail push state
                    , driverActivityStatus state
                    , statsModel push state
                  ]
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity RIGHT
                , orientation VERTICAL
                , weight 1.0
                ][  if not state.props.rideActionModal && (state.props.driverStatusSet == Online || state.props.driverStatusSet == Silent)  then updateLocationAndLastUpdatedView state push else dummyTextView
                  , linearLayout [
                    height WRAP_CONTENT
                    , width MATCH_PARENT
                    , gravity RIGHT
                  ][
                    if (state.props.autoPayBanner /= ST.NO_SUBSCRIPTION_BANNER && state.props.driverStatusSet == ST.Offline && getValueFromConfig "autoPayBanner") then autoPayBannerView state push true else dummyTextView
                    , viewRecenterAndSupport state push
                  ]
                ]
              ]
            , alternateNumberOrOTPView state push
            , if(state.props.showGenderBanner && state.props.driverStatusSet /= ST.Offline && getValueToLocalStore IS_BANNER_ACTIVE == "True" && state.props.autoPayBanner == ST.NO_SUBSCRIPTION_BANNER) then genderBannerView state push 
                else if state.data.paymentState.paymentStatusBanner then paymentStatusBanner state push 
                else if (state.props.autoPayBanner /= ST.NO_SUBSCRIPTION_BANNER && state.props.driverStatusSet /= ST.Offline && getValueFromConfig "autoPayBanner") then autoPayBannerView state push false 
                else if (state.props.driverStatusSet /= ST.Offline && state.props.currentStage == HomeScreen && state.data.config.enablePurpleRideBanner) then accessibilityBanner state push 
                else dummyTextView
            ]
        ]
        , bottomNavBar push state
  ]

accessibilityBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
accessibilityBanner state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin (Margin 10 10 10 10)
  , gravity BOTTOM 
  , weight 1.0 
  ][
    Banner.view (push <<< AccessibilityBannerAction) (accessbilityBannerConfig state )
  ]

genericAccessibilityPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
genericAccessibilityPopUpView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , onClick push (const BackPressed)
  , gravity CENTER
  , background Color.blackLessTrans
  ][ PopUpModal.view (push <<< GenericAccessibilityPopUpAction) (genericAccessibilityPopUpConfig state)]


rateCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ RateCard.view (push <<< RateCardAC) (rateCardState state) ]

paymentStatusBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
paymentStatusBanner state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 10 10 10 10
    , gravity BOTTOM
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity RIGHT
        ][ imageView
            [ height $ V 24
            , width $ V 24
            , gravity RIGHT
            , margin $ MarginRight 4
            , onClick push $ const RemovePaymentBanner
            , imageWithFallback "ny_ic_grey_cross,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_grey_cross_icon.png"
            , visibility if state.data.paymentState.blockedDueToPayment then GONE else VISIBLE
            ]
          , Banner.view (push <<< PaymentBannerAC) (paymentStatusConfig state)
        ]
    ]


alternateNumberOrOTPView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
alternateNumberOrOTPView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , background Color.transparent
  , padding $ PaddingBottom 16
  , gravity BOTTOM
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][  addAlternateNumber push state
        , if (getValueFromConfig "SPECIAL_ZONE_OTP_VIEW") == "true"  then otpButtonView state push else dummyTextView
        ]
      ]

genderBannerView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderBannerView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (Margin 10 10 10 10)
    , visibility if (getValueFromConfig "showGenderBanner") then VISIBLE else GONE
    , gravity BOTTOM
    ][
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity RIGHT
      ][
        imageView
        [
          height $ V 24
        , width $ V 24
        , gravity RIGHT
        , margin (MarginRight 4)
        , onClick push (const RemoveGenderBanner)
        , imageWithFallback "ny_ic_grey_cross,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_grey_cross_icon.png"
        ]
        , genderBanner push state
      ]
    ]

autoPayBannerView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
autoPayBannerView state push configureImage =
  linearLayout
    [ height MATCH_PARENT
    , width  MATCH_PARENT
    , orientation VERTICAL
    , margin (Margin 10 10 10 10)
    , visibility if state.props.autoPayBanner /= ST.NO_SUBSCRIPTION_BANNER then VISIBLE else GONE
    , gravity BOTTOM
    , weight 1.0
    ][
        Banner.view (push <<< AutoPayBanner) (autopayBannerConfig state configureImage)
    ]
  
otpButtonView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
otpButtonView state push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.blue900
    , cornerRadius 32.0
    , background Color.blue600
    , visibility $ if (state.props.statusOnline) then VISIBLE else GONE
    , padding (Padding 16 14 16 14)
    , margin $ MarginLeft 8
    , gravity CENTER_VERTICAL
    , onClick push $ const $ ZoneOtpAction
    ][ imageView
        [ imageWithFallback $ "ic_mode_standby," <> (HU.getAssetStoreLink FunctionCall) <> "ic_mode_standby.png"
        , width $ V 20
        , height $ V 20
        ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , color Color.blue900
        , padding $ PaddingLeft 8
        , margin $ MarginBottom 2
        , text $ getString OTP_
        ] <> FontStyle.subHeading2 TypoGraphy
    ]


cancelConfirmation :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelConfirmation push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalCancelConfirmationAction) (cancelConfirmationConfig state )]

linkAadhaarPopup :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
linkAadhaarPopup push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< LinkAadhaarPopupAC) (linkAadhaarPopupConfig state )]

googleMap :: forall w . HomeScreenState -> PrestoDOM (Effect Unit) w
googleMap state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , id (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap")
  ][]

driverActivityStatus :: forall w . HomeScreenState -> PrestoDOM (Effect Unit) w
driverActivityStatus state =
  linearLayout
  [ width MATCH_PARENT
  , margin $ Margin 10 0 10 0
  , height $ V 1
  , background Color.grey900
  ][]

helpAndSupportBtnView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
helpAndSupportBtnView state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin (Margin 0 0 0 10)
  , cornerRadius 24.0
  ][ imageView
   [ width ( V 42 )
   , height ( V 42 )
   , imageWithFallback "ic_report_help,https://assets.juspay.in/nammayatri/images/common/ic_report_help.png"
   , onClick push (const HelpAndSupportScreen)
   ]
  ]

recenterBtnView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
recenterBtnView state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , stroke $ "1," <> Color.black500
  , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then GONE else VISIBLE
  , cornerRadius 24.0
  ][ imageView
    [ width ( V 40 )
    , height ( V 40 )
    , imageWithFallback $ "ny_ic_recenter_btn," <> (HU.getCommonAssetStoreLink FunctionCall) <> "/ny_ic_recenter_btn.png"
    , onClick (\action -> do
            _ <- JB.getCurrentPosition push CurrentLocation
            pure unit
          ) (const RecenterButtonAction)
    ]
  ]

offlineView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
offlineView push state =
  let showGoInYellow = state.data.config.subscriptionConfig.enableSubscriptionPopups && state.data.paymentState.driverBlocked || (state.data.paymentState.totalPendingManualDues > state.data.config.subscriptionConfig.highDueWarningLimit)
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , gravity BOTTOM
  , background "#2C2F3A80"
  ][ frameLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][ linearLayout
          [ height $ V 280
          , width MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          ][ lottieAnimationView
              [ id (EHC.getNewIDWithTag "RippleGoOnlineLottie")
              , afterRender (\action-> do
                              void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig{ rawJson = "rippling_online_effect.json", lottieId = (EHC.getNewIDWithTag "RippleGoOnlineLottie"), speed = 1.0 }
                            )(const NoAction)
              , height WRAP_CONTENT
              , width MATCH_PARENT
              ]
          ]
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        ][ linearLayout
            [ height $ V 140
            , width MATCH_PARENT
            , gravity BOTTOM
            , background Color.white900
            , PP.cornerRadii $ PTD.Corners 40.0 true true false false
            ][
              textView $
              [
                height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_HORIZONTAL
              , margin $ MarginBottom 10
              , text $ getString if state.data.paymentState.driverBlocked && not state.data.paymentState.subscribed then GO_ONLINE_PROMPT_PAYMENT_PENDING
                                 else if state.data.paymentState.driverBlocked then GO_ONLINE_PROMPT_SUBSCRIBE
                                 else GO_ONLINE_PROMPT
              ] <> FontStyle.paragraphText TypoGraphy
            ]
        ]
    , linearLayout
        [ height $ V 205
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            ][ frameLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , margin $ MarginTop 72
                ][ linearLayout
                    [ height $ V 132
                    , width $ V 132
                    , cornerRadius 75.0
                    , background if showGoInYellow then Color.yellowText else Color.darkMint
                    , onClick  push  (const $ SwitchDriverStatus Online)
                    ][]
                  , textView
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , gravity CENTER
                    , text $ getString GO_ONLINE
                    , textSize FontSize.a_32
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color Color.white900
                    ]
              ]
          ]
      ]
    ]
  ]

popupModelSilentAsk :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
popupModelSilentAsk push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.blackLessTrans
    ][PopUpModal.view (push <<< PopUpModalSilentAction) (silentModeConfig state )]

driverDetail :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverDetail push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , background Color.white900
  , clickable true
  , margin (MarginTop 5)
  ][  linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      , gravity CENTER
      , padding (Padding 16 20 12 16)
      ][ linearLayout [
          width $ V 42
        , height $ V 42
        , onClick push $ const GoToProfile
        ][ imageView
           [ width $ V 42
           , height $ V 42
           , imageWithFallback $ "ny_ic_user," <> HU.getAssetStoreLink FunctionCall <> "ic_new_avatar.png"
           ]
         ]
      ]
    , accessibilityHeaderView push state (getAccessibilityHeaderText state)
    , linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER_HORIZONTAL
      , stroke if state.props.driverStatusSet == Offline then ("2," <> Color.red)
               else if (((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true")&& ((state.props.driverStatusSet == Online) || state.props.driverStatusSet == Silent )) then ("2," <> Color.yellow900)
               else if state.props.driverStatusSet == Online then ("2," <> Color.darkMint)
               else ("2," <> Color.blue800)
      , cornerRadius 50.0
      , alpha if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer])then 0.5 else 1.0
      , margin (Margin 0 10 10 10)
      , visibility if isJust state.data.activeRide.disabilityTag then GONE else VISIBLE 
      ](DA.mapWithIndex (\index item ->
          driverStatusPill item push state index
        ) driverStatusIndicators
      ) 
  ]


accessibilityHeaderView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> ContentConfig -> PrestoDOM (Effect Unit) w
accessibilityHeaderView push state accessibilityHeaderconfig = 
  linearLayout
  [ weight 1.0
  , height MATCH_PARENT
  , gravity CENTER
  , visibility if isJust state.data.activeRide.disabilityTag then VISIBLE else GONE
  , margin (Margin 10 10 10 10)
  , background Color.lightPurple
  , cornerRadius 50.0
  , padding (Padding 8 8 8 8)
  ][
    imageView
    [ width $ V 25
    , imageWithFallback accessibilityHeaderconfig.imageUrl
    , height $ V 22
    , margin $ MarginRight 13
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][ textView $
        [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , padding $ PaddingRight 4
            , text accessibilityHeaderconfig.primaryText
            , color Color.purple
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text accessibilityHeaderconfig.secondaryText
        , color Color.purple
        ] <> FontStyle.body4 TypoGraphy
    ]
  ]

driverStatusPill :: forall w . PillButtonState -> (Action -> Effect Unit) -> HomeScreenState -> Int -> PrestoDOM (Effect Unit) w
driverStatusPill pillConfig push state index =
  linearLayout
  [ weight 1.0
  , height $ V 35
  , gravity CENTER
  , color Color.greyTextColor
  , margin (Margin 10 10 10 10)
  , background $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.background
                    DEMO_ -> Color.yellow900
                    DEFAULT -> Color.white900
  , cornerRadius 50.0
  ][  linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const $ SwitchDriverStatus pillConfig.status)
      , clickable if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then false else true
      ][ imageView
        [ width $ V 15
        , height $ V 15
        , margin (Margin 3 0 5 0)
        , visibility $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> VISIBLE
                    DEMO_ -> VISIBLE
                    DEFAULT -> GONE
        , imageWithFallback $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.imageUrl
                    DEMO_ -> "ic_driver_status_demo,https://assets.juspay.in/beckn/merchantcommon/images/ic_driver_status_demo.png"
                    DEFAULT -> "none"
        ]
      , textView(
        [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding (Padding 0 0 4 0)
          , text $ case pillConfig.status of
              Online -> if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then (getString DEMO) else (getString ONLINE_)
              Offline -> (getString OFFLINE)
              Silent -> (getString SILENT)
          , color $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.textColor
                    DEMO_ -> Color.black900
                    DEFAULT -> Color.greyTextColor
        ] <> FontStyle.body1 TypoGraphy
      )
      ]

  ]

updateLocationAndLastUpdatedView :: forall w . HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateLocationAndLastUpdatedView state push =
  PrestoAnim.animationSet
  [ Anim.translateYAnimFromTop $ AnimConfig.translateYAnimHomeConfig AnimConfig.TOP_BOTTOM ] $
  linearLayout
  [ width MATCH_PARENT
  , padding (Padding 16 8 16 8)
  , margin (Margin 16 16 16 0)
  , cornerRadius 7.0
  , orientation HORIZONTAL
  , background Color.white900
  ][ locationLastUpdatedTextAndTimeView push state
    , updateButtonIconAndText push state
  ]

statsModel :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
statsModel push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then GONE else VISIBLE
    , gravity CENTER
    , padding (Padding 16 10 16 10)
    ][  StatsModel.view (push <<< StatsModelAction) (statsModelConfig state)

    ]

profileDemoView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
profileDemoView state push =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black9000
  , clickable true
  ][ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ Margin 10 20 0 0
      , orientation VERTICAL
      ][ imageView
          [ width $ V 50
          , height $ V 50
          , margin $ Margin 5 5 5 5
          , imageWithFallback "ic_profile_shadow,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_profile_shadow.png"
          , onClick push $ const $ GoToProfile
          ]
        , imageView
          [ width $ V 40
          , height $ V 40
          , margin $ Margin 15 10 0 15
          , imageWithFallback "up_hand_arrow,https://assets.juspay.in/beckn/nammayatri/driver/images/up_hand_arrow.png"
          ]
        , clickHereDemoLayout state push
      ]
  ]

clickHereDemoLayout :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
clickHereDemoLayout state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , background Color.black900
  , stroke $ "1,"<> Color.yellow900
  , margin $ MarginLeft 10
  , cornerRadius 12.0
  ][ textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString CLICK_TO_ACCESS_YOUR_ACCOUNT
      , color Color.white900
      , padding $ Padding 20 12 20 15
      ] <> FontStyle.body13 TypoGraphy
  ]

viewRecenterAndSupport :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
viewRecenterAndSupport state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin (Margin 0 20 20 0)
  , orientation VERTICAL
  , gravity RIGHT
  ][
    -- imageView -- TODO:: ADDING SAFETY/ SUPPORT
    -- [ width ( V 40 )
    -- , height ( V 40 )
    -- , margin $ MarginBottom 10
    -- , imageWithFallback "ny_ic_homepage_support," <> (HU.getCommonAssetStoreLink FunctionCall) <> "ny_ic_homepage_support.png"
    -- -- , onClick (\action -> do
    -- --         _ <- JB.getCurrentPosition push CurrentLocation
    -- --         pure unit
    -- --       ) (const RecenterButtonAction)
    -- ]
    -- ,
    helpAndSupportBtnView state push,
    recenterBtnView state push
  ]
driverStatus :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverStatus push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , gravity RIGHT
  , margin (Margin 0 0 16 0)
  , visibility if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted || state.props.currentStage == ChatWithCustomer ) then GONE else VISIBLE
  ][ linearLayout
     [ width WRAP_CONTENT
     , height MATCH_PARENT
     , gravity CENTER
     , orientation VERTICAL
     ][ imageView
        [ width $ V 50
        , height $ V 30
        , imageWithFallback if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then "ny_ic_demo_mode_switch," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_demo_mode_switch.png" else if state.props.statusOnline then "ny_ic_toggle_on," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_toggle_on.png" else "ny_ic_toggle_off," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_toggle_off.png"
        , margin (MarginTop 10)
        , onClick push (const (ChangeStatus if state.props.statusOnline then false else true))
        , clickable if state.props.rideActionModal then false else true
        ]
      , textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then (getString ONLINE_VIA_DEMO_MODE) else if state.props.statusOnline then (getString ONLINE_) else (getString OFFLINE)
        , color Color.greyTextColor
        ]  <> FontStyle.tags TypoGraphy
        )
      ]
   ]

enterOtpModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
enterOtpModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalAction) (enterOtpStateConfig state)

showOfflineStatus :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
showOfflineStatus push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.black9000
  , gravity BOTTOM
  , visibility if state.props.statusOnline then GONE else VISIBLE
  ][ PrestoAnim.animationSet
    [ Anim.translateYAnim AnimConfig.translateYAnimConfig
    ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , PP.cornerRadii $ PTD.Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding (Padding 0 0 0 20)
      , gravity CENTER
      ][
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , padding (Padding 0 30 0 25)
          ][ imageView
             [ width (V 65)
             , height (V 65)
             , imageWithFallback $ "ny_ic_offline_status," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_offline_status.png"
             ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/5) 0 ((EHC.screenWidth unit)/5) 0)
          ][ textView (
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString YOU_ARE_OFFLINE)
             , color Color.black
             , gravity CENTER
             ]  <> FontStyle.h2 TypoGraphy
             )
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/6) 10 ((EHC.screenWidth unit)/6) 10)
          ][ textView (
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS)
             , gravity CENTER
             , color Color.black600
             ]  <> FontStyle.body1 TypoGraphy
             )
          ]
      ]
  ]

makePaymentModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
makePaymentModal push state = MakePaymentModal.view (push <<< MakePaymentModalAC) (makePaymentState state)


goOfflineModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
goOfflineModal push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , background Color.black9000
  , gravity BOTTOM
  ][
   PrestoAnim.animationSet [
      Anim.translateYAnim AnimConfig.translateYAnimConfig
    ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , PP.cornerRadii $ PTD.Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding (Padding 0 0 0 20)
      , gravity CENTER
      ][
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , padding (Padding 0 30 0 25)
          ][ imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback $ "ic_vehicle_side_active," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_auto_side_active.png"
             ]
           , imageView
             [ width (V 35)
             , height (V 35)
             , margin (Margin 35 0 35 0)
             , imageWithFallback $ "ny_ic_chevrons_right," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_chevrons_right.png"
             ]
           , imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback $ "ic_vehicle_side_inactive," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_auto_side_inactive.png"
             ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/5) 0 ((EHC.screenWidth unit)/5) 0)
          ][ textView $
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE)
             , color Color.black
             , gravity CENTER
             ] <> FontStyle.body14 TypoGraphy
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin (MarginTop 25)
          ][ linearLayout
             [ width (V $ ((EHC.screenWidth unit)/7) * 3)
             , height (V $ 55)
             , stroke ("1," <> Color.black900)
             , cornerRadius 8.0
             , gravity CENTER
             , margin (MarginRight 10)
             , onClick push (const CancelGoOffline)
             ][ textView (
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text (getString CANCEL)
                , color Color.black900
                ]  <> FontStyle.subHeading1 TypoGraphy
                )
             ]
           , linearLayout
             [ width (V $ ((EHC.screenWidth unit)/7) * 3)
             , height (V $ 55)
             , stroke ("1," <> Color.black900)
             , cornerRadius 8.0
             , gravity CENTER
             , margin (MarginRight 10)
             , background Color.black900
             , onClick push (const $ GoOffline if state.props.statusOnline then false else true)
             ][ textView  (
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , text (getString GO_OFFLINE)
                , color Color.yellow900
                ]  <> FontStyle.subHeading1 TypoGraphy
                )
             ]
          ]
      ]
  ]


addAlternateNumber :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
addAlternateNumber push state =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , background Color.white900
  , orientation HORIZONTAL
  , cornerRadius 32.0
  , stroke $ "1," <> Color.black600
  , padding (Padding 20 16 20 16)
  , gravity CENTER_VERTICAL
  , onClick push (const ClickAddAlternateButton)
  , visibility (if ((state.data.driverAlternateMobile == Nothing || state.props.showlinkAadhaarPopup) && (state.props.statusOnline))  then VISIBLE else GONE)
  ][  imageView
      [ width $ V 20
      , height $ V 15
      , imageWithFallback if state.props.showlinkAadhaarPopup then
                            "ny_ic_aadhaar_logo,https://assets.juspay.in/nammayatri/images/driver/ny_ic_aadhaar_logo.png"
                          else
                            "ic_call_plus," <> (HU.getCommonAssetStoreLink FunctionCall) <> "ic_call_plus.png"
      , margin (MarginRight 5)
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , text $ getString if state.props.showlinkAadhaarPopup then ENTER_AADHAAR_DETAILS else ADD_ALTERNATE_NUMBER
      , color Color.black900
      ] <> FontStyle.paragraphText TypoGraphy
   ]

locationLastUpdatedTextAndTimeView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationLastUpdatedTextAndTimeView push state =
  linearLayout
  [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
  ][
    textView $
    [ text $ (getString UPDATED_AT) <> ": "
    , lineHeight "15"
    , color Color.brownishGrey
    , gravity LEFT
    , height WRAP_CONTENT
    ] <> FontStyle.paragraphText TypoGraphy
    , textView $
      [  width WRAP_CONTENT
        , height WRAP_CONTENT
        , ellipsize true
        , singleLine true
        , gravity CENTER_VERTICAL
        , text if state.data.locationLastUpdatedTime == "" then (if (getValueToLocalStore LOCATION_UPDATE_TIME) == "__failed" then getString(NO_LOCATION_UPDATE) else (getValueToLocalStore LOCATION_UPDATE_TIME) ) else state.data.locationLastUpdatedTime
      ] <> FontStyle.body4 TypoGraphy
  ]

updateButtonIconAndText :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
updateButtonIconAndText push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation HORIZONTAL
  , visibility if not state.props.rideActionModal && state.props.statusOnline then VISIBLE else GONE
  , onClick (\action -> do
        _<- push action
        pure unit
        ) (const RetryTimeUpdate)
  , gravity RIGHT
  ]
  [ PrestoAnim.animationSet [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.refreshAnimation)]
    $ imageView
    [ width $ V 20
    , height $ V 20
    , margin $ MarginRight 5
    , imageWithFallback $ "ny_ic_refresh," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_refresh.png"
    , gravity RIGHT
    ],
    textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text (getString UPDATE)
    , color Color.blueTextColor
    , gravity RIGHT
    ] <> FontStyle.body4 TypoGraphy
  ]

waitTimeInfoPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
waitTimeInfoPopUp push state =
   PrestoAnim.animationSet [ Anim.fadeIn true ]
     $ linearLayout
         [ height MATCH_PARENT
         , width MATCH_PARENT
         ]
         [ RequestInfoCard.view (push <<< RequestInfoCardAction) (waitTimeInfoCardConfig FunctionCall) ]

bottomNavBar :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bottomNavBar push state =
    BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.HOME_SCREEN)

rideActionModelView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideActionModelView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , alignParentBottom "true,-1"
  , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted,RideStarted,ChatWithCustomer]) then VISIBLE else GONE
  ][  coordinatorLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][  bottomSheetLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , PP.sheetState COLLAPSED
          , peakHeight $ state.data.peekHeight
          , topShift 0.0
          ][ if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then
                RideActionModal.view (push <<< RideActionModalAction) (rideActionModalConfig state)
                else linearLayout[][]
        ]
      ]
    ]


chatView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
chatView push state =
  PrestoAnim.animationSet [ Anim.translateYAnimFromTop $ AnimConfig.translateFullYAnimWithDurationConfig 300 ] $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  , adjustViewWithKeyboard "true"
  , background Color.transparent
  ][ ChatView.view (push <<< ChatViewActionController) (chatViewConfig state) ]

cancelRidePopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ SelectListModal.view (push <<< CancelRideModalAction) (cancelRideModalConfig state)
    ]

endRidePopView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
endRidePopView push state =
  linearLayout
    [ width MATCH_PARENT,
      height MATCH_PARENT
    ][ PopUpModal.view (push <<<PopUpModalAction) (endRidePopUp state )]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ]

requestInfoCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
requestInfoCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RequestInfoCard.view (push <<< RequestInfoCardAction) (requestInfoCardConfig FunctionCall) ]

enableCurrentLocation :: HomeScreenState -> Boolean
enableCurrentLocation state = if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted]) then false else true


rideStatusPolling :: forall action. String -> Number -> HomeScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
rideStatusPolling pollingId duration state push action = do
  if (getValueToLocalStore RIDE_STATUS_POLLING) == "True" && (getValueToLocalStore RIDE_STATUS_POLLING_ID) == pollingId && (DA.any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithCustomer]) then do
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
    _ <- pure $ spy "polling inside rideStatusPolling function" activeRideResponse
    case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if not (DA.null rideList.list) then do
          void $ delay $ Milliseconds duration
          rideStatusPolling pollingId duration state push action
          else do
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
            if isLocalStageOn HomeScreen then pure unit
              else doAff do liftEffect $ push $ action "CANCELLED_PRODUCT"
      Left err -> pure unit
    else pure unit

rideRequestPolling :: forall action. String -> Int -> Number -> HomeScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
rideRequestPolling pollingId count duration state push action = do
  if (getValueToLocalStore RIDE_STATUS_POLLING) == "True" && (getValueToLocalStore RIDE_STATUS_POLLING_ID) == pollingId && isLocalStageOn RideRequested then do
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
    _ <- pure $ spy "polling inside rideRequestPolling function" activeRideResponse
    case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if (DA.null rideList.list) && count > 0 then do
          void $ delay $ Milliseconds duration
          rideRequestPolling pollingId (count - 1) duration state push action
          else do
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
            if isLocalStageOn RideAccepted then pure unit
              else doAff do liftEffect $ push $ action "DRIVER_ASSIGNMENT"
      Left err -> pure unit
    else pure unit

paymentStatusPooling :: forall action. String -> Int -> Number -> HomeScreenState -> (action -> Effect Unit) -> (APIPaymentStatus -> action) -> Flow GlobalState Unit
paymentStatusPooling orderId count delayDuration state push action = do
  if (getValueToLocalStore PAYMENT_STATUS_POOLING) == "true" && isLocalStageOn HomeScreen && count > 0 && orderId /= "" then do
    orderStatus <- Remote.paymentOrderStatus orderId
    _ <- pure $ spy "polling inside paymentStatusPooling function" orderStatus
    case orderStatus of
      Right (OrderStatusRes resp) -> do
        if (DA.any (_ == resp.status) [CHARGED, AUTHORIZATION_FAILED, AUTHENTICATION_FAILED, JUSPAY_DECLINED]) then do
            _ <- pure $ setValueToLocalStore PAYMENT_STATUS_POOLING "false"
            doAff do liftEffect $ push $ action resp.status
        else do
            void $ delay $ Milliseconds delayDuration
            paymentStatusPooling orderId (count - 1) delayDuration state push action
      Left err -> pure unit
    else pure unit

checkCurrentRide :: forall action.(action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
checkCurrentRide push action = do
  activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
  case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if (DA.null rideList.list) then
          pure unit
          else do
            case (rideList.list DA.!! 0) of
              Just ride -> doAff do liftEffect $ push $ action "DRIVER_ASSIGNMENT"
              Nothing -> pure unit
      Left err -> pure unit

rideRequestPollingData :: RideRequestPollingData
rideRequestPollingData = {
  duration : 18,
  delay : 3000.0
}

getDriverStatusResult :: Int -> DriverStatus -> DriverStatus -> DriverStatusResult
getDriverStatusResult index driverStatus currentStatus= case (getValueToLocalStore IS_DEMOMODE_ENABLED) of
                  "true" -> if(index == 2) then DEMO_
                            else DEFAULT
                  _ -> if (driverStatus == currentStatus) then ACTIVE
                       else DEFAULT

launchMaps :: forall action. (action -> Effect Unit) ->  action -> Flow GlobalState Unit
launchMaps push action = do
  void $ delay $ Milliseconds 2000.0
  if (getValueToLocalStore TRIGGER_MAPS == "true") then
    doAff do liftEffect $ push $ action
    else pure unit
  pure unit

genderBanner :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
genderBanner push state =
  Banner.view (push <<< GenderBannerModal) (genderBannerConfig state)

