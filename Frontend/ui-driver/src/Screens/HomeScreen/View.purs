{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.View where

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar as BottomNavBar
import Components.CancelRide as CancelRide
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.RideActionModal as RideActionModal
import Components.StatsModel as StatsModel
import Components.ChatView as ChatView
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import MerchantConfigs.Utils as MU
import JBridge as JB
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (<<<), (<>), (==), (>), (>=), (||), (<=), show, void, (/=), when)
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (BottomSheetState(..), alignParentBottom, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, peakHeight, stroke, text, textSize, textView, visibility, weight, width, imageWithFallback,adjustViewWithKeyboard,lottieAnimationView,relativeLayout)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (coordinatorLayout)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.HomeScreen.Controller (Action(..), RideRequestPollingData, ScreenOutput, ScreenOutput(GoToHelpAndSupportScreen), checkPermissionAndUpdateDriverMarker, eval)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.Types (HomeScreenStage(..), HomeScreenState, KeyboardModalType(..),DriverStatus(..), DriverStatusResult(..), PillButtonState(..))
import Screens.Types as ST
import Services.APITypes (GetRidesHistoryResp(..))
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore, getValueToLocalNativeStore, isLocalStageOn)
import Styles.Colors as Color
import Types.App (GlobalState)
import Data.Int(toNumber, ceil)
import Control.Monad.Except.Trans (lift)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.APITypes (Status(..))
import Components.BottomNavBar.Controller (navData)
import Screens.HomeScreen.ComponentConfig
import Screens as ScreenNames
import MerchantConfigs.Utils (getValueFromMerchant)
import Engineering.Helpers.Commons (flowRunner)

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
            void $ launchAff $ EHC.flowRunner $ runExceptT $ runBackT $ do
              (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "1" "0" "true" "null"
              case (activeRideResponse.list DA.!! 0) of
                Just ride -> lift $ lift $ doAff do liftEffect $ push $ RideActiveAction ride
                Nothing -> setValueToLocalStore IS_RIDE_ACTIVE "false"

          case (getValueToLocalNativeStore LOCAL_STAGE) of
            "RideRequested"  -> do
                                if (getValueToLocalStore RIDE_STATUS_POLLING) == "False" then do
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "True"
                                  let secondsOver = if EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true >= (rideRequestPollingData.duration) then (rideRequestPollingData.duration) else EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true
                                      counts = ceil $ (toNumber (rideRequestPollingData.duration - secondsOver) * 1000.0)/rideRequestPollingData.delay
                                  if counts > 0 then do
                                      void $ launchAff $ EHC.flowRunner $ rideRequestPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) counts rideRequestPollingData.delay initialState push Notification
                                  else
                                      void $ pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
                                  pure unit
                                  else pure unit
            "RideAccepted"   -> do
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "2000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "5.0"
                                if (not initialState.props.chatcallbackInitiated) then do
                                  _ <- JB.storeCallBackMessageUpdated push initialState.data.activeRide.id "Driver" UpdateMessages
                                  _ <- JB.startChatListenerService
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
                                  _ <- launchAff $ EHC.flowRunner $ rideStatusPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) 20000.0 initialState push Notification
                                  pure unit
                                  else pure unit
            "RideStarted"    -> do
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "50000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "25.0"
                                _ <- push RemoveChat
                                _ <- launchAff $ flowRunner $ launchMaps push TriggerMaps
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  _ <- JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
                                  pure unit 
                                  else pure unit 
            "ChatWithCustomer" -> do 
                                if (initialState.data.activeRide.notifiedCustomer && (not initialState.props.updatedArrivalInChat)) then do
                                  _ <- pure $ JB.sendMessage (getEN I_HAVE_ARRIVED)
                                  push UpdateInChat
                                  else pure unit
            _                -> do
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "50000"
                                _ <- pure $ JB.removeAllPolylines ""
                                _ <- JB.reallocateMapFragment (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap")
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "25.0"
                                _ <- pure $ setValueToLocalStore SESSION_ID (JB.generateSessionId unit)
                                _ <- checkPermissionAndUpdateDriverMarker initialState
                                _ <- launchAff $ EHC.flowRunner $ checkCurrentRide push Notification
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
  ]([  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , PP.sheetState EXPANDED
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
          linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , weight 1.0
            , orientation VERTICAL
            , background Color.white900
            ,cornerRadius 50.0
            ][  linearLayout
                [ width MATCH_PARENT
                , height $ V 2
                , background Color.greyTextColor
                , visibility if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then GONE else VISIBLE
                , alpha 0.1
                ][]
              , frameLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                ][  googleMap state
                  -- , linearLayout
                  --   [ height MATCH_PARENT
                  --   , width MATCH_PARENT
                  --   , background Color.blackLessTrans
                  --   ][]
                  , if state.props.driverStatusSet == ST.Offline then offlineView push state else dummyTextView
                  , linearLayout
                   [ width MATCH_PARENT
                   , height WRAP_CONTENT
                   , orientation VERTICAL ]
                   [ linearLayout
                      [
                        width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation VERTICAL
                      , PP.cornerRadii $ PTD.Corners 24.0  false false true true
                      , background $ Color.white900
                      , stroke $ (if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then "0," else "1,") <> "#E5E7EB"
                      ][
                        driverDetail2 push state
                        , driverActivityStatus state
                        -- , updateLocationAndLastUpdatedView2 state push
                        , statsModel2 push state
                      ]
                  -- , if not state.props.statusOnline then showOfflineStatus push state else dummyTextView
                    , linearLayout[
                      width MATCH_PARENT
                      , height WRAP_CONTENT
                      , gravity RIGHT
                      , orientation VERTICAL
                      , weight 1.0
                      ][
                        if not state.props.rideActionModal && (state.props.driverStatusSet == ST.Online || state.props.driverStatusSet == ST.Silent)  then updateLocationAndLastUpdatedView2 state push else dummyTextView
                        , viewRecenterAndSupport state push
                      ]
                    ]
                  , alternateNumberOrOTPView state push
                  ]
              ]
        , bottomNavBar push state
        ]
      , if (getValueToLocalNativeStore PROFILE_DEMO) /= "false" then profileDemoView state push else linearLayout[][]
      , if state.props.goOfflineModal then goOfflineModal push state else dummyTextView
      , if state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted then  rideActionModelView push state else dummyTextView
      , if state.props.enterOtpModal then enterOtpModal push state else dummyTextView
      , if state.props.endRidePopUp then endRidePopView push state else dummyTextView
      , if state.props.currentStage == ChatWithCustomer then chatView push state else dummyTextView
      , if state.props.silentPopUpView then popupModelSilentAsk push state else dummyTextView
      ] <> if state.props.cancelRideModalShow then [cancelRidePopUpView push state] else []
        <>  if state.props.cancelConfirmationPopup then [cancelConfirmation push state] else []
        )

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
        , if (getValueFromMerchant "SPECIAL_ZONE_OTP_VIEW") == "true"  then otpButtonView state push else dummyTextView
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
        [ imageWithFallback "ic_mode_standby,https://assets.juspay.in/nammayatri/images/user/ic_mode_standby.png"
        , width $ V 20
        , height $ V 20
        ]
      , textView $ 
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , gravity CENTER
        , color Color.blue900
        , padding $ PaddingLeft 8
        , margin $ MarginBottom 2
        , text $ getString OTP_
        , textSize FontSize.a_14
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
  , margin (Margin 10 0 10 0)
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
  , visibility if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then GONE else VISIBLE
  , cornerRadius 24.0
  ][ imageView
    [ width ( V 40 )
    , height ( V 40 )
    , imageWithFallback "ny_ic_recenter_btn,https://assets.juspay.in/nammayatri/images/common/ny_ic_recenter_btn.png"
    , onClick (\action -> do
            _ <- JB.getCurrentPosition push CurrentLocation
            pure unit
          ) (const RecenterButtonAction)
    ]
  ]

offlineView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
offlineView push state =
  linearLayout[
          width MATCH_PARENT
        , height MATCH_PARENT--(V 200)
        , gravity BOTTOM
        , background "#2C2F3A80"
        ][
          frameLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ][
            linearLayout
            [ height (V 280)
            , width MATCH_PARENT
            , gravity CENTER_HORIZONTAL
            ][ lottieAnimationView
              [ id (EHC.getNewIDWithTag "RippleGoOnlineLottie")
              , afterRender (\action-> do
                              _ <- pure $ JB.startLottieProcess "rippling_online_effect" (EHC.getNewIDWithTag "RippleGoOnlineLottie") true 1.0 "DEFAULT"
                              pure unit)(const NoAction)
              , height WRAP_CONTENT
              , width MATCH_PARENT
              ]
            ]
          , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , gravity BOTTOM
            ][
          linearLayout
            [ height (V 140)
            , width MATCH_PARENT
            , gravity BOTTOM
            , background Color.white900
            , PP.cornerRadii $ PTD.Corners 40.0 true true false false
            ][
              textView
              [
                height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_HORIZONTAL
              , margin $ MarginBottom 10
              , text $ getString GO_ONLINE_PROMPT
              , textSize FontSize.a_14
              ]
            ]
            ]
         , linearLayout
            [ height (V 205)
            , width MATCH_PARENT
            , gravity CENTER_HORIZONTAL
            ][ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              -- , gravity CENTER
              ][
                frameLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , margin $ MarginTop 72
                ][linearLayout
                  [ height $ V 132
                  , width $ V 132
                  , cornerRadius 75.0
                  , background "#53BB6F"
                  , onClick  push  (const $ SwitchDriverStatus ST.Online)
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
  , margin (MarginTop 5)
  ][  linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      , padding $ Padding 16 20 12 16
      ][ imageView
         [ width $ V 42
         , height $ V 42
         , imageWithFallback "ny_ic_avatar,https://assets.juspay.in/nammayatri/images/driver/ny_ic_avatar.png"
         ]
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , padding (Padding 0 18 0 16)
      ][  textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text ((DS.take 12 (getValueToLocalStore USER_NAME)) <> (if (DS.length (getValueToLocalStore USER_NAME)) > 12 then "..." else "")) --------
          , color Color.greyTextColor
          ] <> FontStyle.subHeading1 TypoGraphy
          )
        , textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text state.data.vehicleType
          ] <> FontStyle.body3 TypoGraphy
          )
      ]
    , driverStatus push state
  ]

driverDetail2 :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverDetail2 push state =
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
      , stroke if state.props.driverStatusSet == Offline then ("2," <> Color.red)
               else if (((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true")&& ((state.props.driverStatusSet == Online) || state.props.driverStatusSet == Silent )) then ("2," <> Color.yellow900)
               else if state.props.driverStatusSet == Online then ("2," <> Color.darkMint)
               else ("2," <> Color.blue800)
      , cornerRadius 50.0
      , alpha if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted || state.props.currentStage == ChatWithCustomer) then 0.5 else 1.0
      , margin (Margin 20 10 20 10)--padding (Padding 10 10 10 10)
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
      , clickable if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted || state.props.currentStage == ChatWithCustomer) then false else true
      ][ imageView
        [ width $ V 15
        , height $ V 15
        , margin (MarginRight 5)
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
          , text $ case pillConfig.status of
              Online -> if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then (getString DEMO) else (getString ONLINE_)
              Offline -> (getString OFFLINE)
              Silent -> (getString SILENT)
          , color $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.textColor
                    DEMO_ -> Color.black900
                    DEFAULT -> Color.greyTextColor
          , fontStyle $ FontStyle.medium LanguageStyle
          , textSize FontSize.a_14
        ]
      )
      ]

  ]

updateLocationAndLastUpdatedView2 :: forall w . HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateLocationAndLastUpdatedView2 state push =
  PrestoAnim.animationSet
  [ Anim.translateYAnimFromTop $ AnimConfig.translateYAnimHomeConfig AnimConfig.TOP_BOTTOM ] $
  linearLayout
  [ width MATCH_PARENT
  -- , height $ V 40
  , padding (Padding 16 8 16 8)
  , margin (Margin 16 16 16 0)
  , cornerRadius 7.0
  , orientation HORIZONTAL
  , background Color.white900
  ][ locationLastUpdatedTextAndTimeView push state
    , updateButtonIconAndText push state
--    , helpAndSupportBtnView state push
    -- , recenterBtnView state push
  ]

statsModel2 :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
statsModel2 push state =
  -- PrestoAnim.animationSet
  -- [ Anim.translateYAnimFromTop $ AnimConfig.translateYAnimHomeConfig AnimConfig.TOP_BOTTOM ] $
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , visibility if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then GONE else VISIBLE
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
  ][ textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString CLICK_TO_ACCESS_YOUR_ACCOUNT
      , color Color.white900
      , textSize FontSize.a_18
      , fontStyle $ FontStyle.medium TypoGraphy
      , padding $ Padding 20 12 20 15
      ]
  ]

viewRecenterAndSupport :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
viewRecenterAndSupport state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin (Margin 0 20 20 0)
  , orientation VERTICAL
  ][
    -- imageView -- TODO:: ADDING SAFETY/ SUPPORT
    -- [ width ( V 40 )
    -- , height ( V 40 )
    -- , margin $ MarginBottom 10
    -- , imageWithFallback "ny_ic_homepage_support,https://assets.juspay.in/nammayatri/images/common/ny_ic_homepage_support.png"
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
        , imageWithFallback if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then "ny_ic_demo_mode_switch,https://assets.juspay.in/nammayatri/images/driver/ny_ic_demo_mode_switch.png" else if state.props.statusOnline then "ny_ic_toggle_on,https://assets.juspay.in/nammayatri/images/driver/ny_ic_toggle_on.png" else "ny_ic_toggle_off,https://assets.juspay.in/nammayatri/images/driver/ny_ic_toggle_off.png"
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
             , imageWithFallback "ny_ic_offline_status,https://assets.juspay.in/nammayatri/images/driver/ny_ic_offline_status.png"
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
             , imageWithFallback "ic_vehicle_side_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_auto_side_active.png"
             ]
           , imageView
             [ width (V 35)
             , height (V 35)
             , margin (Margin 35 0 35 0)
             , imageWithFallback "ny_ic_chevrons_right,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevrons_right.png"
             ]
           , imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback "ic_vehicle_side_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_auto_side_inactive.png"
             ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/5) 0 ((EHC.screenWidth unit)/5) 0)
          ][ textView
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE)
             , textSize FontSize.a_19
             , color Color.black
             , gravity CENTER
             ]
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

updateLocationAndLastUpdatedView :: forall w . HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateLocationAndLastUpdatedView state push =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 16 8 16 8)
  , orientation HORIZONTAL
  ][ locationLastUpdatedTextAndTimeView push state
   , updateButtonIconAndText push state
  ]

-- driverDetailsView state push =
--  linearLayout
--   [ width MATCH_PARENT
--   , height $ V 50
--   , padding (Padding 16 8 16 8)
--   , orientation HORIZONTAL
--   ][ locationLastUpdatedTextAndTimeView push state
--    , updateButtonIconAndText push state
--   ]
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
      , imageWithFallback "ic_call_plus,https://assets.juspay.in/nammayatri/images/driver/ic_call_plus.png"
      , margin (MarginRight 5)
      ] 
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , text (getString ADD_ALTERNATE_NUMBER)       
      , color Color.black900
      , textSize FontSize.a_14
      ]
   ]

locationLastUpdatedTextAndTimeView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationLastUpdatedTextAndTimeView push state =
  linearLayout
  [ height MATCH_PARENT
    , width WRAP_CONTENT
  ][
    textView
    [ text $ (getString UPDATED_AT) <> ": "
    , textSize FontSize.a_14
    , lineHeight "15"
    , color Color.brownishGrey
    , gravity LEFT
    , fontStyle $ FontStyle.regular LanguageStyle
    ]
    , textView
    [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text if state.data.locationLastUpdatedTime == "" then (if (getValueToLocalStore LOCATION_UPDATE_TIME) == "__failed" then getString(NO_LOCATION_UPDATE) else (getValueToLocalStore LOCATION_UPDATE_TIME) ) else state.data.locationLastUpdatedTime
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.bold LanguageStyle
    ]
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
    , imageWithFallback "ny_ic_refresh,https://assets.juspay.in/nammayatri/images/driver/ny_ic_refresh.png"
    , gravity RIGHT
    ],
    textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text (getString UPDATE)
    , color Color.blueTextColor
    , textSize FontSize.a_14
    , gravity RIGHT
    , fontStyle $ FontStyle.bold LanguageStyle
    ]
  ]

statsModel :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
statsModel push state =
  PrestoAnim.animationSet
  [ Anim.translateYAnimFromTop $ AnimConfig.translateYAnimHomeConfig AnimConfig.TOP_BOTTOM ] $
  linearLayout
  [ orientation VERTICAL
  , width MATCH_PARENT
  , height MATCH_PARENT
  , gravity RIGHT
  , margin (Margin 16 10 16 0)
  ][ linearLayout
   [ orientation VERTICAL
   , width MATCH_PARENT
   , gravity RIGHT
   , visibility if not state.props.rideActionModal && state.props.statusOnline then VISIBLE else GONE
   ][ StatsModel.view (push <<< StatsModelAction) (statsModelConfig state)
    , helpAndSupportBtnView state push
    , recenterBtnView state push
   ]
   , if state.props.rideActionModal && state.props.statusOnline then helpAndSupportBtnView state push else dummyTextView
  ]

bottomNavBar :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bottomNavBar push state = 
    BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.HOME_SCREEN)

rideActionModelView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideActionModelView push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted,RideStarted]) then VISIBLE else GONE
  ][  coordinatorLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ][  bottomSheetLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , PP.sheetState COLLAPSED
          , peakHeight if state.data.activeRide.isDriverArrived then 518 else 470
          , halfExpandedRatio 0.9
          ][ RideActionModal.view (push <<< RideActionModalAction) (rideActionModalConfig state)]
        ]
    ]

chatView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
chatView push state =
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
    ][ CancelRide.view (push <<< CancelRideModalAction) (cancelRideModalConfig state)
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