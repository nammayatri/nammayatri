{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FollowRideScreen.View where

import Accessor (_lat, _lon)
import Animation
import Animation.Config
import Common.Resources.Constants (pickupZoomLevel, zoomLevel, chatService)
import Common.Types.App (LazyCheck(..), Paths, City(..))
import Components.DriverInfoCard.Common.View (addressShimmerView, driverDetailsView, driverInfoShimmer, sourceDestinationView)
import Constants.Configs (getPolylineAnimationConfig)
import Data.Array (any, head, length, mapWithIndex, (!!), notElem, elem)
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, runFn1)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Uncurried (runEffectFn1, runEffectFn10)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, getValueFromIdMap, liftFlow, os, updatePushInIdMap, safeMarginTopWithDefault, screenWidth, safeMarginBottomWithDefault, safeMarginTop)
import Helpers.Utils (FetchImageFrom(..), fetchImage, storeCallBackCustomer, makeNumber, getDefaultPixelSize, getRouteMarkers, normalRoute, TrackingType(..))
import JBridge (animateCamera, drawRoute, enableMyLocation, getExtendedPath, isCoordOnPath, removeAllPolylines, removeMarker, showMap, updateRoute, updateRouteConfig, startChatListenerService, stopChatListenerService, storeCallBackMessageUpdated, storeCallBackOpenChatScreen, clearChatMessages, getKeyInSharedPrefKeys, scrollOnResume, setMapPadding, getLayoutBounds, defaultMarkerConfig, mkRouteConfig)
import JBridge as JB
import Mobility.Prelude (boolToVisibility)
import Prelude
import PrestoDOM (PrestoDOM, Screen, BottomSheetState(..), onAnimationEnd, onBackPressed, onClick, afterRender)
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout, frameLayout, imageView, linearLayout, relativeLayout, scrollView, textView)
import PrestoDOM.Properties (alignParentBottom, accessibility, alpha, background, clickable, color, cornerRadii, cornerRadius, enableShift, gradient, gravity, height, id, imageWithFallback, margin, orientation, padding, peakHeight, stroke, sheetState, text, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Accessiblity(..), Corners(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..))
import Screens.FollowRideScreen.Controller (Action(..), ScreenOutput, eval, isMockDrill, localDelay, getPoint, getPeekHeight, getSosStatus, getChatChannelId)
import Screens.FollowRideScreen.ScreenData (mockDriverInfo, mockDriverLocation, mockRoute)
import Screens.FollowRideScreen.Config (SOSOverlayConfig, genericHeaderConfig, getCurrentFollower, getDriverDetails, getFollowerName, getSosOverlayConfig, getTripDetails, primaryButtonConfig, getChatSuggestions)
import Services.API (GetDriverLocationResp(..), GetRouteResp(..), LatLong(..), RideBookingRes(..), Route(..), Snapped(..))
import Services.Backend (drawMapRoute, getDriverLocation, getRoute, makeGetRouteReq, rideBooking, walkCoordinate, walkCoordinates)
import Types.App (GlobalState(..), defaultGlobalState)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Data.String (take)
import Font.Style as FontStyle
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (Flow, getState, modifyState)
import Helpers.Pooling (delay)
import PrestoDOM.Animation as PrestoAnim
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Screens.Types (DriverInfoCard, EmAudioPlayStatus(..), FollowRideScreenStage(..), FollowRideScreenState, Followers, Stage(..))
import Styles.Colors as Color
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.RippleCircles (addAndUpdateRideSafeOverlay, addAndUpdateSOSRipples, addNavigateMarker, removeSOSRipples)
import LocalStorage.Cache (getValueFromCache)
import Storage
import Components.MessagingView as MessagingView
import Locale.Utils (getLanguageLocale, languageKey)
import Common.Resources.Constants (emChatSuggestion)
import Components.MessagingView.Common.View (messageNotificationView)
import Components.MessagingView.Common.Types (MessageNotificationView)
import Engineering.Helpers.LogEvent (logEventWithMultipleParams)
import Foreign (unsafeToForeign)
import Helpers.SpecialZoneAndHotSpots (zoneLabelIcon)
import Screens.Types as ST
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import Components.Safety.Utils as SU
import Components.MessagingView.Controller as CMC
import Services.API as API

screen :: FollowRideScreenState -> GlobalState -> Screen Action FollowRideScreenState ScreenOutput
screen initialState globalState =
  { initialState
  , view: (view globalState)
  , name: "FollowRideScreen"
  , globalEvents:
      [ ( \push -> do
            storeCallBackCustomer push NotificationListener "FollowRideScreen" Just Nothing
            tracking <- runEffectFn1 getValueFromIdMap "FollowsRide"
            case initialState.data.currentStage of
              MockFollowRide -> void $ launchAff $ flowRunner globalState $ updateMockData push initialState tracking.id
              PersonList -> pure unit
              _ -> do
                _ <- pure $ enableMyLocation false
                let
                  currentFollower = getCurrentFollower initialState.data.currentFollower
                logEventWithMultipleParams initialState.data.logField "ny_user_following_ride"
                  $ [ { key: "BookingId", value: unsafeToForeign $ currentFollower.bookingId }
                    , { key: "FollowerId", value: unsafeToForeign $ getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys }
                    ]
                case initialState.data.driverInfoCardState, not initialState.props.chatCallbackInitiated of
                  Just ride, true -> do
                    if os /= "IOS" then do
                      checkChatService push 5 ride.rideId initialState
                    else if os == "IOS" then do
                      startChatServices push ride.rideId initialState
                    else
                      pure unit
                  _, _ -> pure unit
                when tracking.shouldPush $ void $ launchAff $ flowRunner globalState $ driverLocationTracking push UpdateStatus 2000.0 tracking.id "trip"
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "FollowRideScreen action " action
        let
          _ = spy "FollowRideScreen state " state
        eval action state
  }

checkChatService :: forall w. (Action -> Effect Unit) -> Int -> String -> FollowRideScreenState -> Effect Unit
checkChatService push retry rideId state =
  when (retry > 0)
    $ do
        let
          isChatServiceRunning = runFn1 JB.isServiceRunning chatService
        if isChatServiceRunning then do
          void $ pure $ delay $ Milliseconds 2000.0
          checkChatService push (retry - 1) rideId state
        else
          startChatServices push rideId state

startChatServices :: (Action -> Effect Unit) -> String -> FollowRideScreenState -> Effect Unit
startChatServices push rideId state = do
  let
    customerId = getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys

    channelId = getChatChannelId state
  void $ clearChatMessages
  void $ storeCallBackMessageUpdated push channelId customerId UpdateMessages AllChatsLoaded
  void $ storeCallBackOpenChatScreen push OpenChatScreen
  void $ startChatListenerService
  void $ scrollOnResume push ScrollToBottom
  push InitializeChat

type Layout w
  = PrestoDOM (Effect Unit) w

view ::
  forall w.
  GlobalState ->
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
view globalState push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        ]
    $ case state.data.currentStage of
        PersonList -> [ followersListView push state ]
        _ -> [ followingRideView globalState push state ]

followingRideView ::
  forall w.
  GlobalState ->
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
followingRideView globalState push state =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    $ [ PrestoAnim.animationSet
          [ fadeIn state.props.startMapAnimation
          ]
          $ linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , id $ getNewIDWithTag "FollowRideMap"
              , onAnimationEnd
                  getPush
                  (const NoAction)
              ]
              []
      , relativeLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          ]
          [ messageWidgetView push state
          , bottomSheetView push state
          ]
      , rideCompletedView push state
      ]
    <> if state.data.currentStage == ChatWithEM then
        [ messagingView push state ]
      else
        []
          <> if state.data.currentStage == RideCompletedStage then
              []
            else case state.data.sosStatus of
              Just status -> case status of
                Common.NotResolved -> [ genericHeaderView push state ]
                _ -> [ sosOverlayView push (getSosOverlayConfig state status) ]
              Nothing -> [ genericHeaderView push state ]
  where
  getPush =
    ( \action -> do
        void $ showMap (getNewIDWithTag "FollowRideMap") true "satellite" pickupZoomLevel 0.0 0.0 getMapReadyPush MapReady
        push action
    )

  getMapReadyPush =
    ( \action -> do
        tracking <- runEffectFn1 getValueFromIdMap "FollowsRide"
        when tracking.shouldPush $ void $ launchAff $ flowRunner globalState $ driverLocationTracking push UpdateStatus 2000.0 tracking.id "trip"
        push action
    )

genericHeaderView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
genericHeaderView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ PaddingTop safeMarginTop
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state) ]

followersListView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
followersListView push state =
  let
    len = length state.data.followers
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ]
      [ genericHeaderView push state
      , scrollView
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ]
          [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              ]
              ( mapWithIndex
                  ( \idx item ->
                      linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        $ [ followerItem item push ]
                        <> if idx /= (len - 1) then [ separatorView ] else []
                  )
                  state.data.followers
              )
          ]
      ]

messageWidgetView :: forall w. (Action -> Effect Unit) -> FollowRideScreenState -> PrestoDOM (Effect Unit) w
messageWidgetView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ ( if disableSuggestions state then
          PrestoAnim.animationSet []
        else
          ( if state.props.showChatNotification then
              PrestoAnim.animationSet [ translateYAnimFromTop $ messageInAnimConfig true ]
            else if state.props.isNotificationExpanded then
              PrestoAnim.animationSet [ translateYAnimFromTop $ messageOutAnimConfig true ]
            else
              PrestoAnim.animationSet [ scaleYAnimWithDelay 5000 ]
          )
      )
        $ linearLayout
            [ height $ MATCH_PARENT
            , width MATCH_PARENT
            , padding $ PaddingHorizontal 8 8
            , alignParentBottom "true,-1"
            , gravity BOTTOM
            , accessibility DISABLE
            , onAnimationEnd push $ const $ NotificationAnimationEnd
            , orientation VERTICAL
            ]
            [ messageNotificationView push (getMessageNotificationViewConfig state)
            , linearLayout
                [ height $ V $ ((getPeekHeight state) - if state.props.removeNotification then 0 else 180)
                , width $ MATCH_PARENT
                , accessibility DISABLE
                ]
                []
            ]
    ]
  where
  disableSuggestions state = not $ state.data.config.feature.enableChat || state.data.config.feature.enableSuggestions

rideCompletedView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
rideCompletedView push state =
  let
    currentFollower = getCurrentFollower state.data.currentFollower

    name = getFollowerName currentFollower state

    titleText = getString $ if state.data.sosStatus /= Just Common.Resolved then RIDE_ENDED name else REACHED_DESTINATION_SAFELY name
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.blackLessTrans
      , clickable true
      , visibility $ boolToVisibility $ state.data.currentStage == RideCompletedStage
      , gravity BOTTOM
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.white900
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , padding $ PaddingHorizontal 16 16
          ]
          [ imageView
              [ height $ V 82
              , width $ V 82
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
              , margin $ MarginTop 24
              ]
          , textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text titleText
                , color Color.black900
                , margin $ MarginTop 24
                , gravity CENTER
                ]
              <> FontStyle.h1 TypoGraphy
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , margin $ MarginTop 32
              ]
              [ sourceDestinationView push (getTripDetails state Color.blue600) ]
          , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
          ]
      ]

followerItem :: forall w. Followers -> (Action -> Effect Unit) -> Layout w
followerItem item push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , background Color.white900
    , onClick push $ const (PersonSelected item)
    , gravity CENTER
    ]
    [ linearLayout
        [ height $ V 32
        , width $ V 32
        , gravity CENTER
        , cornerRadius 16.0
        , background Color.blue800
        ]
        [ textView
            $ [ text $ take 1 $ fromMaybe item.mobileNumber item.name
              , color Color.white900
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              ]
            <> FontStyle.tags TypoGraphy
        ]
    , textView
        $ [ text $ fromMaybe item.mobileNumber item.name
          , color Color.black800
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginLeft 8
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        ]
        []
    , imageView
        [ height $ V 20
        , width $ V 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
        ]
    ]

getMessageNotificationViewConfig :: FollowRideScreenState -> MessageNotificationView Action
getMessageNotificationViewConfig state =
  let
    config = MessagingView.config

    driverInfoCard = fromMaybe mockDriverInfo state.data.driverInfoCardState

    currentFollower = getCurrentFollower state.data.currentFollower

    name = fromMaybe currentFollower.mobileNumber currentFollower.name

    removeNotification =
      if state.props.isRideStarted then
        state.props.removeNotification
      else
        true

    toChatComponent { message, sentBy, timeStamp, type: type_, delay } = { message, sentBy, timeStamp, type: type_, delay }
  in
    { showChatNotification: state.props.showChatNotification
    , enableChatWidget: state.props.enableChatWidget
    , isNotificationExpanded: state.props.isNotificationExpanded
    , fareProductType: driverInfoCard.fareProductType
    , config: state.data.config
    , rideStarted: true
    , lastMessage: toChatComponent state.data.lastMessage
    , lastSentMessage: state.data.lastSentMessage
    , lastReceivedMessage: state.data.lastReceivedMessage
    , removeNotificationAction: RemoveNotification
    , messageViewAnimationEnd: MessageViewAnimationEnd
    , messageReceiverAction: MessageEmergencyContact
    , sendQuickMessageAction: SendQuickMessage
    , timerCounter: state.data.counter
    , messageExpiryAction: MessageExpiryTimer
    , chatSuggestions: getChatSuggestions state
    , messages: state.data.messages
    , removeNotification: removeNotification
    , currentStage: RideStarted
    , suggestionKey: emChatSuggestion
    , user:
        { userName: name
        , receiver: name
        }
    , isOtpRideFlow: false
    }

bottomSheetView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
bottomSheetView push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      ]
      [ coordinatorLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [ bottomSheetLayout
              ( [ height WRAP_CONTENT
                , width MATCH_PARENT
                , peakHeight $ getPeekHeight state
                , enableShift false
                ]
                  <> maybe [] (\sheet -> [ sheetState sheet ]) state.props.sheetState
              )
              [ linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  [ dismissAlarmView push state
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , background Color.grey700
                      , orientation VERTICAL
                      , gravity CENTER
                      , cornerRadii $ Corners 24.0 true true false false
                      ]
                      [ linearLayout
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , orientation VERTICAL
                          , gravity CENTER
                          , id $ getNewIDWithTag "FollowRideHeaderView"
                          ]
                          [ knobView
                          , headerView push state
                          , emergencyActionsView push state
                          ]
                      , addressView push state
                      , if isJust state.data.driverInfoCardState then
                          driverInfoView push state
                        else
                          PrestoAnim.animationSet [ fadeOut isRideData ]
                            $ linearLayout
                                [ width MATCH_PARENT
                                , height WRAP_CONTENT
                                , visibility $ boolToVisibility $ not $ isJust state.data.driverInfoCardState
                                ]
                                [ driverInfoShimmer
                                ]
                      ]
                  ]
              ]
          ]
      ]

dismissAlarmView :: forall w. (Action -> Effect Unit) -> FollowRideScreenState -> PrestoDOM (Effect Unit) w
dismissAlarmView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , visibility $ boolToVisibility $ any (_ == state.data.emergencyAudioStatus) [ STARTED, RESTARTED ]
    , padding $ PaddingBottom 10
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.white900
        , cornerRadius 20.0
        , padding $ Padding 20 12 20 12
        , onClick push $ const StopAudioPlayer
        , gravity CENTER
        ]
        [ imageView
            [ height $ V 20
            , width $ V 20
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_alarm_new"
            ]
        , textView
            $ [ text $ getString TURN_OFF_ALARM
              , color Color.black800
              ]
            <> FontStyle.tags TypoGraphy
        ]
    ]

messagingView :: forall w. (Action -> Effect Unit) -> FollowRideScreenState -> PrestoDOM (Effect Unit) w
messagingView push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ state.data.currentStage == ChatWithEM
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , margin $ MarginBottom $ (getChatHeight state) + 135
        ]
        [ dismissAlarmView push state ]
    , MessagingView.view (push <<< MessagingViewAC) $ messagingViewConfig state
    ]

sosOverlayView :: forall w. (Action -> Effect Unit) -> SOSOverlayConfig -> Layout w
sosOverlayView push config =
  let
    currentGradient = Linear 180.0 [ Color.white900, Color.white900, Color.white900, Color.transparent ]
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ Padding 16 (safeMarginTopWithDefault 10) 16 70
      , gravity CENTER
      , gradient currentGradient
      , clickable true
      , onClick (\_ -> pure unit) $ const NoAction
      ]
      [ linearLayout
          [ width $ V $ (screenWidth unit) - 16
          , height $ V 34
          , gravity RIGHT
          ]
          [ linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , onClick push $ const DismissOverlay
              , padding $ Padding 5 5 5 5
              ]
              [ imageView
                  [ height $ V 24
                  , width $ V 24
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                  ]
              ]
          ]
      , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ config.title
            , color config.color
            , gravity CENTER
            ]
          <> FontStyle.h2 TypoGraphy
      , textView
          $ [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ config.subTitle
            , gravity CENTER
            , margin $ MarginTop 6
            , color config.color
            ]
          <> FontStyle.body1 TypoGraphy
      ]

knobView :: forall w. Layout w
knobView =
  linearLayout
    [ height $ V 4
    , width $ V 34
    , background Color.transparentGrey
    , margin $ MarginTop 8
    , cornerRadius 4.0
    ]
    []

driverInfoView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
driverInfoView push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ]
      [ PrestoAnim.animationSet [ fadeIn isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility isRideData
              , padding $ PaddingBottom $ safeMarginBottomWithDefault (if os == "IOS" then 48 else 32)
              ]
              [ driverDetailsView (push <<< DriverInfoCardAction) (getDriverDetails state) "FollowRideDriverDetailsView" "FollowNumberPlate"
              ]
      ]

driverInfoViewShimmer ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
driverInfoViewShimmer push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ]
      [ PrestoAnim.animationSet [ fadeOut isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility $ not $ isJust state.data.driverInfoCardState
              ]
              [ driverInfoShimmer
              ]
      ]

addressView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
addressView push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , id $ getNewIDWithTag "FollowRideSourceDestinationView"
      ]
      [ PrestoAnim.animationSet [ fadeIn isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility isRideData
              ]
              [ sourceDestinationView push (getTripDetails state Color.white900)
              ]
      , PrestoAnim.animationSet [ fadeOut isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility $ not isRideData
              ]
              [ addressShimmerView
              ]
      ]

headerView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
headerView push state =
  let
    currentFollower = getCurrentFollower state.data.currentFollower
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 16 12 16 6
      , gravity CENTER
      ]
      [ textView
          $ [ text
                $ if state.props.isRideStarted || state.data.currentStage == MockFollowRide then
                    (getFollowerName currentFollower state) <> " " <> getString IS_ON_THE_WAY
                  else
                    getString $ YET_TO_START (getFollowerName currentFollower state)
            , height WRAP_CONTENT
            , color Color.black900
            ]
          <> FontStyle.body7 TypoGraphy
      , linearLayout
          [ weight 1.0 ]
          []
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          , cornerRadius if os == "IOS" then 20.0 else 32.0
          , background Color.green100
          , onClick push $ const $ if isChatEnabled currentFollower then MessageEmergencyContact else MessagingViewAC (MessagingView.Call)
          , accessibility ENABLE
          , alpha $ if state.props.isMock then 0.5 else 1.0
          , clickable $ not state.props.isMock
          , padding $ Padding 20 10 20 10
          ]
          [ imageView
              [ imageWithFallback $ if isChatEnabled currentFollower then if state.props.unReadMessages then fetchImage FF_ASSET "ic_chat_badge_green" else fetchImage FF_ASSET "ic_call_msg" else fetchImage FF_COMMON_ASSET "ny_ic_call"
              , height $ V if isChatEnabled currentFollower then state.data.config.driverInfoConfig.callHeight else 20
              , width $ V if isChatEnabled currentFollower then state.data.config.driverInfoConfig.callWidth else 20
              ]
          ]
      ]
  where
  isChatEnabled currentFollower = state.data.config.feature.enableChat && not currentFollower.isManualFollower

emergencyActionsView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
emergencyActionsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 10 12 10 12
    , margin $ Margin 16 6 16 12
    , cornerRadius 8.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        ( map
            ( \item ->
                SafetyActionTileView.view item.image item.text item.push item.backgroundColor item.strokeColor (V $ (screenWidth unit - 76) / 2) true item.isDisabled Color.black900 WRAP_CONTENT
            )
            safetyActions
        )
    ]
  where
  safetyActions =
    [ { text: getStringWithoutNewLine CALL_POLICE, image: "ny_ic_ny_support", backgroundColor: Color.white900, strokeColor: Color.grey900, push: push <<< CallPolice, isDisabled: state.props.isMock }
    , { text: getStringWithoutNewLine CALL_SAFETY_TEAM, image: "ny_ic_help_and_support_dark", backgroundColor: Color.white900, strokeColor: Color.grey900, push: push <<< CallSafetyTeam, isDisabled: state.props.isMock }
    ]

buttonView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
buttonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , cornerRadius 24.0
    , gravity CENTER
    , padding $ Padding 16 12 16 12
    , onClick push $ const $ CallPolice SafetyActionTileView.OnClick
    , alpha $ if state.props.isMock then 0.5 else 1.0
    , clickable $ not state.props.isMock
    ]
    [ imageView
        [ width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_police_black"
        , margin $ MarginRight 10
        ]
    , textView
        $ [ text $ getString CALL_POLICE
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.black800
          ]
        <> FontStyle.tags TypoGraphy
    ]

driverLocationTracking :: (Action -> Effect Unit) -> (RideBookingRes -> Action) -> Number -> Int -> String -> Flow GlobalState Unit
driverLocationTracking push action duration id routeState = do
  (GlobalState gs) <- getState
  let
    state = gs.followRideScreen
  trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
  when (id == trackingId.id)
    $ do
        when (isJust state.data.currentFollower)
          $ do
              let
                follower = getCurrentFollower state.data.currentFollower
              void $ pure $ runFn2 updatePushInIdMap "FollowsRide" false
              resp <- rideBooking follower.bookingId
              case resp of
                Right respBooking -> do
                  updateSosStatus respBooking
                  liftFlow $ push $ action respBooking
                  when ((getPeekHeight state) <= 156) $ liftFlow $ push $ UpdatePeekHeight
                Left err -> pure unit
        (GlobalState gs) <- getState
        let
          state = gs.followRideScreen
        case state.data.driverInfoCardState of
          Nothing -> pure unit
          Just ride -> do
            response <- getDriverLocation ride.rideId
            case response of
              Right resp -> do
                trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
                when (id == trackingId.id)
                  $ do
                      case state.data.route of
                        Nothing -> routeNotExist resp ride state
                        Just route -> routeExist resp route ride state
              Left _ -> do
                void $ delay $ Milliseconds $ duration * 2.0
                resetRoute
                driverLocationTracking push action duration id routeState
  where
  resetRoute :: Flow GlobalState Unit
  resetRoute =
    void
      $ modifyState \(GlobalState globalState) ->
          GlobalState
            $ globalState
                { followRideScreen
                  { data
                    { route = Nothing
                    }
                  }
                }

  updateSosStatus :: RideBookingRes -> Flow GlobalState Unit
  updateSosStatus (RideBookingRes resp) =
    void
      $ modifyState \(GlobalState globalState) ->
          GlobalState
            $ globalState
                { followRideScreen
                  { data
                    { sosStatus = getSosStatus resp.sosStatus resp.id
                    }
                  }
                }

  routeNotExist :: GetDriverLocationResp -> DriverInfoCard -> FollowRideScreenState -> Flow GlobalState Unit
  routeNotExist (GetDriverLocationResp resp) ride state = do
    let
      srcLat = (resp ^. _lat)

      srcLon = (resp ^. _lon)

      dstLat = ride.destinationLat

      dstLon = ride.destinationLng

      destination = ride.destination

      markers = getRouteMarkers ride.vehicleVariant state.props.city RIDE_TRACKING ride.fareProductType (Just RideStarted)

      sourceSpecialTagIcon = zoneLabelIcon state.data.zoneType.sourceTag

      destSpecialTagIcon = zoneLabelIcon state.data.zoneType.destinationTag

      specialLocationTag = HSConfig.specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
    routeResponse <- getRoute routeState $ makeGetRouteReq srcLat srcLon dstLat dstLon Nothing
    case routeResponse of
      Right (GetRouteResp routeResp) -> do
        case ((routeResp) !! 0) of
          Just (Route routes) -> do
            void $ pure $ removeAllPolylines ""
            void $ liftFlow $ removeSOSRipples ""
            let
              (Snapped routePoints) = routes.points

              newPoints =
                if length routePoints > 1 then
                  getExtendedPath (walkCoordinates routes.points)
                else
                  walkCoordinate srcLat srcLon dstLat dstLon

              newRoute = routes { points = Snapped (map (\item -> LatLong { lat: item.lat, lon: item.lng }) newPoints.points) }

              point = { lat: srcLat, lng: srcLon }

              srcMarkerConfig = defaultMarkerConfig { markerId = markers.srcMarker, pointerIcon = markers.srcMarker }

              destMarkerConfig = defaultMarkerConfig { markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = ride.destination, anchorV = 1.0 }
            addSosMarkers state.data.sosStatus point
            let
              normalRoute = mkRouteConfig newPoints srcMarkerConfig destMarkerConfig Nothing "DRIVER_LOCATION_UPDATE" "LineString" true JB.DEFAULT specialLocationTag
            liftFlow $ drawRoute [ normalRoute ] (getNewIDWithTag "FollowRideMap") -- check this
            liftFlow $ animateCamera srcLat srcLon 16.0 "ZOOM"
            void $ delay $ Milliseconds duration
            void
              $ modifyState \(GlobalState globalState) ->
                  GlobalState
                    $ globalState
                        { followRideScreen
                          { data
                            { route = Just (Route newRoute)
                            , speed = routes.distance / routes.duration
                            }
                          }
                        }
            driverLocationTracking push action duration id routeState
          Nothing -> do
            void $ delay $ Milliseconds $ duration * 2.0
            driverLocationTracking push action duration id routeState
      Left _ -> do
        void $ delay $ Milliseconds $ duration * 2.0
        driverLocationTracking push action duration id routeState

  routeExist :: GetDriverLocationResp -> Route -> DriverInfoCard -> FollowRideScreenState -> Flow GlobalState Unit
  routeExist (GetDriverLocationResp resp) (Route route) ride state = do
    let
      srcLat = (resp ^. _lat)

      srcLon = (resp ^. _lon)

      dstLat = ride.destinationLat

      dstLon = ride.destinationLng

      markers = getRouteMarkers ride.vehicleVariant state.props.city RIDE_TRACKING ride.fareProductType Nothing

      sourceSpecialTagIcon = zoneLabelIcon state.data.zoneType.sourceTag

      destSpecialTagIcon = zoneLabelIcon state.data.zoneType.destinationTag

      specialLocationTag = HSConfig.specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
    locationResp <- liftFlow $ isCoordOnPath (walkCoordinates route.points) (resp ^. _lat) (resp ^. _lon) (state.data.speed)
    if locationResp.isInPath then do
      let
        newPoints = { points: locationResp.points }

        mbPoint = head locationResp.points

        destMarkerConfig = defaultMarkerConfig { markerId = markers.destMarker, pointerIcon = markers.destMarker }
      liftFlow
        $ runEffectFn1 updateRoute
            updateRouteConfig { json = newPoints, destMarkerConfig = destMarkerConfig, eta = (HSConfig.metersToKm locationResp.distance true), srcMarker = markers.srcMarker, specialLocation = specialLocationTag, zoomLevel = zoomLevel, autoZoom = false, polylineKey = "DEFAULT", pureScriptID = (getNewIDWithTag "FollowRideMap") }
      case mbPoint of
        Just point -> addSosMarkers state.data.sosStatus point
        Nothing -> pure unit
      liftFlow $ animateCamera srcLat srcLon 16.0 "ZOOM"
      void $ delay $ Milliseconds duration
      driverLocationTracking push action duration id routeState
    else do
      resetRoute
      driverLocationTracking push action duration id routeState

  addSosMarkers :: Maybe Common.SosStatus -> Paths -> Flow GlobalState Unit
  addSosMarkers mbStatus point = case mbStatus of
    Nothing -> pure unit
    Just status -> case status of
      _
        | elem status [ Common.Resolved, Common.MockResolved ] -> do
          _ <- pure $ removeMarker "SOSMarkerLabel"
          liftFlow $ addAndUpdateRideSafeOverlay point
      _
        | elem status [ Common.Pending, Common.MockPending ] -> do
          liftFlow $ addAndUpdateSOSRipples point
          liftFlow $ addNavigateMarker point push OnNavigate
      _ -> pure unit

separatorView :: forall w. PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.greySmoke
    ]
    []

updateMockData :: (Action -> Effect Unit) -> FollowRideScreenState -> Int -> Flow GlobalState Unit
updateMockData push state id = defaultMockInviteFlow id state
  -- rideBookingListResponse <- rideBookingList "1" "0" "true" -- Required when we show actual data
  -- case rideBookingListResponse of
  --   Right (RideBookingListRes listResp) -> do
  --     if null listResp.list then
  --       defaultMockFlow
  --     else do
  --       let
  --         mbRideBooking = head listResp.list
  --       case mbRideBooking of
  --         Nothing -> defaultMockFlow
  --         Just ride -> do
  --           let
  --             driverInfoCardState = getDriverInfo Nothing ride false
  --           eiResp <- getDriverLocation driverInfoCardState.rideId
  --           case eiResp of
  --             Right resp -> currentRideDataMockFlow driverInfoCardState resp
  --             Left _ -> defaultMockFlow
  --   Left _ -> defaultMockFlow
  where
  defaultMockInviteFlow :: Int -> FollowRideScreenState -> Flow GlobalState Unit
  defaultMockInviteFlow id state = do
    localDelay 1000.0
    let
      srcPoint = getPoint mockDriverLocation
    when (state.data.sosStatus == Just Common.MockPending)
      $ do
          pushSOSStatus state.data.sosStatus
          liftFlow $ addAndUpdateSOSRipples srcPoint
    pushAction $ UpdateMockData mockDriverInfo { vehicleDetails = "AUTO_RICKSHAW", vehicleVariant = "AUTO_RICKSHAW" }
    drawDriverRoute mockDriverInfo srcPoint (Just mockRoute) false
    localDelay 180000.0
    trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
    when (id == trackingId.id)
      $ do
          pushAction BackPressed

  currentRideDataMockFlow :: DriverInfoCard -> GetDriverLocationResp -> Flow GlobalState Unit
  currentRideDataMockFlow ride resp = do
    let
      srcPoint = getPoint resp
    pushAction $ UpdateMockData ride
    drawDriverRoute ride srcPoint Nothing false
    pushSOSStatus $ Just Common.Pending
    localDelay 10000.0
    liftFlow $ addAndUpdateRideSafeOverlay srcPoint
    pushSOSStatus $ Just Common.Resolved
    localDelay 10000.0
    pushAction $ UpdateCurrentStage RideCompletedStage

  pushAction :: Action -> Flow GlobalState Unit
  pushAction action = liftFlow $ push $ action

  pushSOSStatus :: Maybe Common.SosStatus -> Flow GlobalState Unit
  pushSOSStatus status = liftFlow $ push $ UpdateMockSOSStatus status

  drawDriverRoute :: DriverInfoCard -> Paths -> Maybe Route -> Boolean -> Flow GlobalState Unit
  drawDriverRoute ride srcPoint route showRipples = do
    let
      markers = normalRoute ""

      srcMarkerConfig = defaultMarkerConfig { markerId = markers.srcMarker, pointerIcon = markers.srcMarker, primaryText = getString SOS_LOCATION }

      destMarkerConfig = defaultMarkerConfig { markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = getString DROP }
    void $ runExceptT $ runBackT $ drawMapRoute srcPoint.lat srcPoint.lng ride.destinationLat ride.destinationLng srcMarkerConfig destMarkerConfig "NORMAL" route "trip" $ (HSConfig.specialLocationConfig "" "" false getPolylineAnimationConfig) { autoZoom = false }
    when showRipples
      $ do
          liftFlow $ addAndUpdateSOSRipples srcPoint
    liftFlow $ animateCamera srcPoint.lat srcPoint.lng 16.0 "ZOOM"

messagingViewConfig :: FollowRideScreenState -> MessagingView.Config
messagingViewConfig state =
  let
    config = MessagingView.config

    driverInfoCard = fromMaybe mockDriverInfo state.data.driverInfoCardState

    currentFollower = getCurrentFollower state.data.currentFollower

    name = fromMaybe currentFollower.mobileNumber currentFollower.name

    toChatComponent { message, sentBy, timeStamp, type: type_, delay } = { message, sentBy, timeStamp, type: type_, delay }
  in
    MessagingView.config
      { userConfig
        { userName = name
        , receiver = name
        }
      , feature
        { sendMessageActive = state.props.sendMessageActive
        , canSendSuggestion = state.props.canSendSuggestion
        , showAutoGeneratedText = false
        , enableSuggestions = state.data.config.feature.enableSuggestions
        , showVehicleDetails = false
        }
      , messages = map toChatComponent state.data.messages
      , messagesSize = state.data.messagesSize
      , vehicleNo = makeNumber $ driverInfoCard.registrationNumber
      , chatSuggestionsList = getChatSuggestions state
      , hint = (getString MESSAGE)
      , languageKey = (getLanguageLocale languageKey)
      , rideConfirmedAt = driverInfoCard.startedAt
      , autoGeneratedText = ""
      , driverRating = show $ driverInfoCard.rating
      , fareAmount = show $ driverInfoCard.price
      , config = state.data.config
      , peekHeight = getChatHeight state
      , otp = driverInfoCard.otp
      , suggestionKey = emChatSuggestion
      , currentChatRecipient =
        { name: name
        , number: currentFollower.mobileNumber
        , uuid: driverInfoCard.rideId <> "$" <> getValueToLocalStore CUSTOMER_ID
        , recipient: CMC.USER
        , enableForShareRide: false
        , contactPersonId: Nothing
        , notifiedViaFCM: Nothing
        , shareTripWithEmergencyContactOption: API.NEVER_SHARE
        }
      }

getChatHeight :: FollowRideScreenState -> Int
getChatHeight state =
  let
    srcAndDstBounds = runFn1 getLayoutBounds $ getNewIDWithTag "FollowRideSourceDestinationView"
  in
    getPeekHeight state + getDefaultPixelSize srcAndDstBounds.height
