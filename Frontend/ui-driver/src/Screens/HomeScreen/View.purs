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

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..))
import Common.Types.App as Common
import Components.Banner.Controller as BannerConfig
import Components.Banner.View as Banner
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.ChatView as ChatView
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.SavedLocationCard as SavedLocationCard
import Components.SelectListModal as SelectListModal
import Components.StatsModel as StatsModel
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int (ceil, toNumber)
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Utils (getValueFromConfig)
import MerchantConfig.Utils as MU
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (<<<), (<>), (==), (>), (>=), (||), (<=), show, void, (/=), when, map, otherwise)
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (BottomSheetState(..), alignParentBottom, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, peakHeight, stroke, text, textSize, textView, visibility, weight, width, imageWithFallback, adjustViewWithKeyboard, lottieAnimationView, relativeLayout, scrollView, scrollBarY, ellipsize, singleLine)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (coordinatorLayout)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Prim.TypeError (class Warn)
import Screens as ScreenNames
import Screens.HomeScreen.Controller (Action(..), RideRequestPollingData, ScreenOutput, ScreenOutput(GoToHelpAndSupportScreen), checkPermissionAndUpdateDriverMarker, eval)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.Types (HomeScreenStage(..), HomeScreenState, KeyboardModalType(..), DriverStatus(..), DriverStatusResult(..), PillButtonState(..), SavedLocationScreenType(..), TimerStatus(..))
import Screens.Types as ST
import Services.API (GetRidesHistoryResp(..))
import Services.API (Status(..))
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore, getValueToLocalNativeStore, isLocalStageOn, setValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState)
import Types.App (defaultGlobalState)

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
          when (getValueToLocalNativeStore IS_RIDE_ACTIVE == "true" && initialState.data.activeRide.status == NOTHING) do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
              (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "1" "0" "true" "null"
              case (activeRideResponse.list DA.!! 0) of
                Just ride -> lift $ lift $ doAff do liftEffect $ push $ RideActiveAction ride
                Nothing -> setValueToLocalStore IS_RIDE_ACTIVE "false"
          let startingTime = (HU.differenceBetweenTwoUTC (HU.getCurrentUTC "") (getValueToLocalStore SET_WAITING_TIME))
          if ((getValueToLocalStore IS_WAIT_TIMER_STOP) == "Triggered") && initialState.props.timerRefresh  then do
            _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP (show (PostTriggered))
            _ <- JB.waitingCountdownTimer startingTime push WaitTimerCallback
            pure unit
          else pure unit
          -- if state.props.enableGotoTimer
          --   then pure unit
          --   else pure unit 
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
                                  _ <- JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
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
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  _ <- JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
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
                                pure unit
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
  ][ relativeLayout
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
      , if (getValueToLocalNativeStore PROFILE_DEMO) /= "false" then profileDemoView state push else linearLayout[][]
      , if state.props.goOfflineModal then goOfflineModal push state else dummyTextView
      , if state.props.enterOtpModal then enterOtpModal push state else dummyTextView
      , if state.props.endRidePopUp then endRidePopView push state else dummyTextView
      , if state.props.cancelConfirmationPopup then cancelConfirmation push state else dummyTextView
      , if state.props.cancelRideModalShow then cancelRidePopUpView push state else dummyTextView
      , if state.props.currentStage == ChatWithCustomer then chatView push state else dummyTextView
      , if state.props.showBonusInfo then requestInfoCardView push state else dummyTextView
      , if state.props.silentPopUpView then popupModelSilentAsk push state else dummyTextView
      , gotoModel push state
      , if state.props.goToInfo then gotoKnowMoreView push state else dummyTextView
      , if state.data.activeRide.waitTimeInfo then waitTimeInfoPopUp push state else dummyTextView
      --, gotoRequestPopupConfigView push state
      --, gotoCancellationPreventionView push state
      --, gotoLocInRangeView push state 
  ]

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
            , if state.props.driverStatusSet == Offline then offlineView push state else dummyTextView
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
                  , viewRecenterAndSupport state push
                ]
              ]
            , alternateNumberOrOTPView state push
            , if(state.props.showGenderBanner && state.props.driverStatusSet /= ST.Offline && getValueToLocalStore IS_BANNER_ACTIVE == "True") then genderBannerView state push else linearLayout[][]
            ]
        ]
        , bottomNavBar push state
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
        [ imageWithFallback $ "ic_mode_standby," <> (getAssetStoreLink FunctionCall) <> "ic_mode_standby.png"
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
  [ width $ V 130
  , height $ V 42
  , orientation HORIZONTAL
  , margin (Margin 0 5 25 10)
  , cornerRadius 22.0
  , onClick push (const HelpAndSupportScreen)
  , background Color.white900
  , gravity CENTER
  , stroke $ "1,"<> Color.black500
  ][ imageView
     [ width ( V 23 )
     , height ( V 23 )
     , imageWithFallback "ny_ic_vector,https://assets.juspay.in/nammayatri/images/common/ic_report_help.png"
     , padding $ PaddingHorizontal 5 0
     ]
   , textView
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , text "Report Issue"
     , gravity CENTER 
     , padding $ PaddingHorizontal 0 10
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
  , margin $ MarginVertical 5 0
  ][ imageView
    [ width ( V 40 )
    , height ( V 40 )
    , imageWithFallback $ "ny_ic_recenter_btn," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_recenter_btn.png"
    , onClick (\action -> do
            _ <- JB.getCurrentPosition push CurrentLocation
            pure unit
          ) (const RecenterButtonAction)
    ]
  ]

offlineView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
offlineView push state =
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
              , text $ getString GO_ONLINE_PROMPT
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
                    , background Color.darkMint
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
      , padding (Padding 16 20 12 16)
      ][ imageView
         [ width $ V 42
         , height $ V 42
         , onClick push $ const GoToProfile
         , imageWithFallback "ic_new_avatar,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_new_avatar.png"
         ]
      ]
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
      ](DA.mapWithIndex (\index item ->
          driverStatusPill item push state index
        ) driverStatusIndicators
      )
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
  , margin (Margin 20 20 20 0)
  , orientation HORIZONTAL
  ][
    -- imageView -- TODO:: ADDING SAFETY/ SUPPORT
    -- [ width ( V 40 )
    -- , height ( V 40 )
    -- , margin $ MarginBottom 10
    -- , imageWithFallback "ny_ic_homepage_support," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_homepage_support.png"
    -- -- , onClick (\action -> do
    -- --         _ <- JB.getCurrentPosition push CurrentLocation
    -- --         pure unit
    -- --       ) (const RecenterButtonAction)
    -- ]
    -- ,
    frameLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 0 25
    ] [ gotoButton push state 
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][ textView
           [ height $ V 20
           , width $ V 20
           , cornerRadius 37.0
           , text $ show state.data.gotoCount 
           , textSize FontSize.a_12
           , color Color.white900
           , gravity CENTER
           , background Color.black900 
           ]
         ]
      ]
      , helpAndSupportBtnView state push
      , recenterBtnView state push
  ]

gotoButton :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoButton push state = 
  linearLayout
  [ width $ V 130
  , height $ V 42
  , orientation HORIZONTAL
  , margin (MarginVertical 5 10)
  , cornerRadius 22.0
  , clickable gotoConfig.clickableVal
  , onClick push (const GoToEnable)
  , background  gotoTimer.bgColor 
  , gravity CENTER
  , stroke $ "1,"<> Color.black500
  ][ linearLayout
      [height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER
      ][ imageView
        [ width ( V 15)
        , height ( V 15 )
        , imageWithFallback gotoTimer.imageString
        , margin $ MarginHorizontal 0 5
        , alpha gotoConfig.alphaVal
        ]
      ]
   , textView
     [ height MATCH_PARENT
     , width WRAP_CONTENT
     , text $ getString GO_TO
     , color gotoTimer.textColor
     , gravity CENTER
     , alpha gotoConfig.alphaVal 
     ]       
  ]
  where gotoConfig = getGotoConfig state.data.gotoCount
        gotoTimer = gotoTimerConfig state.props.enableGotoTimer

gotoTimerConfig :: Boolean -> {bgColor :: String , imageString :: String, textColor :: String }
gotoTimerConfig timer 
  | timer = {bgColor : Color.green900, imageString : "ny_ic_goto_icon_map_pin_check,https://assets.juspay.in/nammayatri/images/common/ic_report_help.png", textColor : Color.white900}
  | otherwise = {bgColor : Color.white900, imageString : "ny_ic_disabled_goto,https://assets.juspay.in/nammayatri/images/common/ic_report_help.png", textColor : Color.black800}

getGotoConfig :: Int -> {alphaVal:: Number, clickableVal :: Boolean}
getGotoConfig val 
  | val == 0 = {alphaVal : 0.3, clickableVal : false}
  | otherwise = {alphaVal : 1.0 , clickableVal : true}

gotoModel :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w 
gotoModel push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , clickable true
  , visibility if state.props.gotoVisibility then VISIBLE else GONE
  , margin (Margin 0 15 0 0)
  ][ header state push 
   , linearLayout
     [ height $ V 1
     , width MATCH_PARENT
     , background Color.grey900
     ] [ ]
   , linearLayout
     [ height $ V 50  
     , width MATCH_PARENT
     , background Color.blue600
     , padding $ PaddingLeft 10
     , orientation VERTICAL
     , gravity CENTER
     ][ textView
        [ width MATCH_PARENT
        , text "Choose a Go-To location "
        ]
      ]  
   , savedLocationListView push state
   , linearLayout
      [ width MATCH_PARENT
      , orientation VERTICAL
      , gravity BOTTOM
      , weight 1.0
      ] [
        textView
          [ height $ V 45
          , width MATCH_PARENT
          , gravity CENTER
          , background Color.blue600
          , stroke $ "1,"<>Color.blue600
          , margin $ Margin 17 0 17 20 
          , text "You have only __ left for today "
          , cornerRadius 6.0 
          , color Color.black900
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin $ MarginVertical 0 20
           ] [ textView
              [ height $ V 45
              , width $ V 160
              , background Color.white900
              , stroke $ "1,"<>Color.grey800
              , margin $ MarginHorizontal 17 17
              , text "Cancel"
              , onClick push $ const CancelBack 
              , cornerRadius 6.0 
              , color Color.black900
              , gravity CENTER
              ]
            , textView
              [ height $ V 45
              , width $ V 160
              , background Color.black900
              , stroke $ "1,"<>Color.black800
              , margin $ MarginHorizontal 17 17
              , text "Yes Enable"
              , onClick push $ const EnableGotoTimer 
              , color Color.yellow900
              , cornerRadius 6.0
              , gravity CENTER 
              ] 
            ]
          ]
   ]

savedLocationListView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
savedLocationListView push state =
  scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarY true
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 8)
          , orientation VERTICAL
          ](map (\item -> 
          SavedLocationCard.view (push <<< FavouriteLocationAC) (item) 
          ) (getFavourites))] 

getFavourites :: Array Common.LocationListItemState
getFavourites = [
  {
    prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : true
  , title : "HOME"
  , subTitle : "Stiring"
  , placeId : Nothing
  , lat : Nothing
  , lon : Nothing
  , description : "ADDRESS"
  , tag : "String"
  , tagType : Nothing
  , cardType : Nothing
  , address : "String"
  , tagName : "String"
  , isEditEnabled : true
  , savedLocation : "String"
  , placeName : "String"
  , isClickable : true
  , alpha : 1.0
  , fullAddress : address'
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Nothing
  , editAcText : Nothing
  , removeAcText : Nothing
  , radioButtonVisibility : true
  }
]

address' :: Common.Address 
address' = {
   area : Just "String"
  , state : Just "String"
  , country : Just "String"
  , building  :Just "String"
  , door : Just "String"
  , street : Just "String"
  , city : Just "String"
  , areaCode :Just "String"
  , ward : Just "String"
  , placeId : Just "String"
}

header :: forall w . HomeScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 10 7 10 10
  , background Color.white900 
  ] [ imageView 
      [ height MATCH_PARENT
      , width $ V 40
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
      , onClick push $ const BackPressed
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , padding $ PaddingLeft 10
      ] [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text "Enable Go-To"
          , gravity LEFT
          , color Color.black900
          ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding $ PaddingLeft 30
      , onClick push $ const ClickInfo
      ] [ textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text "Know More"
          , gravity RIGHT
          , color Color.blue900
          ]
        ]  

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
        , imageWithFallback if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then "ny_ic_demo_mode_switch," <> (getAssetStoreLink FunctionCall) <> "ny_ic_demo_mode_switch.png" else if state.props.statusOnline then "ny_ic_toggle_on," <> (getAssetStoreLink FunctionCall) <> "ny_ic_toggle_on.png" else "ny_ic_toggle_off," <> (getAssetStoreLink FunctionCall) <> "ny_ic_toggle_off.png"
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
             , imageWithFallback $ "ny_ic_offline_status," <> (getAssetStoreLink FunctionCall) <> "ny_ic_offline_status.png"
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
             , imageWithFallback $ "ic_vehicle_side_active," <> (getAssetStoreLink FunctionCall) <> "ny_ic_auto_side_active.png"
             ]
           , imageView
             [ width (V 35)
             , height (V 35)
             , margin (Margin 35 0 35 0)
             , imageWithFallback $ "ny_ic_chevrons_right," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevrons_right.png"
             ]
           , imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback $ "ic_vehicle_side_inactive," <> (getAssetStoreLink FunctionCall) <> "ny_ic_auto_side_inactive.png"
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
  , visibility (if ((state.data.driverAlternateMobile == Nothing) && (state.props.statusOnline))  then VISIBLE else GONE)
  ][  imageView
      [ width $ V 20
      , height $ V 15
      , imageWithFallback $ "ic_call_plus," <> (getCommonAssetStoreLink FunctionCall) <> "ic_call_plus.png"
      , margin (MarginRight 5)
      ] 
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , text (getString ADD_ALTERNATE_NUMBER)
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
    , width $ V (JB.getWidthFromPercent 32)
    , gravity LEFT
    , height WRAP_CONTENT
    ] <> FontStyle.paragraphText TypoGraphy
    , textView $
    [ width $ V (JB.getWidthFromPercent 30)
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
    , imageWithFallback $ "ny_ic_refresh," <> (getAssetStoreLink FunctionCall) <> "ny_ic_refresh.png"
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
          , peakHeight if state.data.activeRide.isDriverArrived then 518 else 470
          , halfExpandedRatio 0.9
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


gotoKnowMoreView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoKnowMoreView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GotoKnowMoreAction) (gotoKnowMoreConfig state)] 

gotoRequestPopupConfigView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoRequestPopupConfigView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GotoRequestPopupAction) (gotoRequestPopupConfig state)]



gotoCancellationPreventionView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoCancellationPreventionView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GotoCancellationPreventionAction) (gotoCancellationPreventionConfig state)]

gotoLocInRangeView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoLocInRangeView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GotoLocInRangeAction) (gotoLocInRangeConfig state)]


enableCurrentLocation :: HomeScreenState -> Boolean
enableCurrentLocation state = if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted]) then false else true


rideStatusPolling :: forall action. String -> Number -> HomeScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
rideStatusPolling pollingId duration state push action = do
  if (getValueToLocalStore RIDE_STATUS_POLLING) == "True" && (getValueToLocalStore RIDE_STATUS_POLLING_ID) == pollingId && (DA.any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithCustomer]) then do
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null"
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
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null"
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

checkCurrentRide :: forall action.(action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
checkCurrentRide push action = do
  activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null"
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