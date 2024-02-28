{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FollowRideScreen.Controller where

import Accessor (_lat, _lon)
import Data.Array (elem, last, length, filter)
import Data.Function.Uncurried (runFn3)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText, updateIdMap, flowRunner, liftFlow)
import Engineering.Helpers.Suggestions (getSuggestionsfromKey, emChatSuggestion)
import JBridge (clearAudioPlayer, getChatMessages, hideKeyboardOnNavigation, scrollToEnd, sendMessage, showDialer, startAudioPlayer, stopChatListenerService, getKeyInSharedPrefKeys, toast)
import Prelude
import PrestoDOM (BottomSheetState(..), Eval, continue, continueWithCmd, defaultPerformLog, exit, updateAndExit)
import Screens.HomeScreen.Transformer (getDriverInfo, getSpecialTag)
import Screens.Types (DriverInfoCard, EmAudioPlayStatus(..), FollowRideScreenStage(..), FollowRideScreenState)
import Services.API (RideBookingRes(..), Route, GetDriverLocationResp(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, setValueToLocalStore)
import Common.Types.App (LazyCheck(..), SosStatus(..), Paths)
import Components.DriverInfoCard as DriverInfoCard
import Components.GenericHeader as GenericHeader
import Components.MessagingView as MessagingView
import Data.Lens ((^.))
import Data.String as STR
import Effect.Aff (Milliseconds(..), launchAff)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.RippleCircles (addAndUpdateRideSafeOverlay, addAndUpdateSOSRipples, addNavigateMarker, removeSOSRipples)
import Helpers.Utils (performHapticFeedback)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Presto.Core.Types.Language.Flow (Flow, delay, getState, modifyState)
import Screens.Types as ST
import SessionCache (getSosAlarmStatus, removeSOSAlarmStatus, setSosAlarmStatus, sosAlarmStatus)
import Screens.FollowRideScreen.ScreenData (dummyFollower, mockFollower)
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried (runEffectFn1)
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))
import Screens.FollowRideScreen.ScreenData (mockDriverInfo, mockDriverLocation, mockRoute)
import Effect (Effect(..))
import Debug
import LocalStorage.Cache (getValueFromCache)
import Language.Strings (getString)
import Language.Types (STR(..))
import Timers (clearTimerWithId)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = Exit FollowRideScreenState
  | RestartTracking FollowRideScreenState
  | OpenNavigation FollowRideScreenState

data Action
  = AfterRender
  | GenericHeaderAC GenericHeader.Action
  | DriverInfoCardAC DriverInfoCard.Action
  | NoAction
  | MapReady String String String
  | UpdateStatus RideBookingRes
  | PersonSelected ST.Followers
  | UpdateMessages String String String String
  | LoadMessages
  | OpenChatScreen
  | MessageEmergencyContact
  | MessagingViewAC MessagingView.Action
  | MessageViewAnimationEnd
  | NotificationAnimationEnd
  | RemoveNotification
  | CollapseBottomSheet
  | CallEmergencyContact
  | SendQuickMessage String
  | RemoveChat
  | InitializeChat
  | ScrollToBottom
  | BackPressed
  | CallPolice
  | StopAudioPlayer
  | StartAudioPlayer
  | OnAudioCompleted String
  | NotificationListener String
  | DismissOverlay
  | UpdateRoute Route
  | UpdatePeekHeight
  | OnNavigate
  | UpdateMockData DriverInfoCard
  | UpdateMockSOSStatus (Maybe SosStatus)
  | PrimaryButtonAC PrimaryButton.Action
  | UpdateCurrentStage FollowRideScreenStage
  | ResetSheetState
  | MessageExpiryTimer Int String String
  | AllChatsLoaded

eval :: Action -> FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
eval action state = case action of
  BackPressed -> do 
    case state.data.currentStage of
      ChatWithEM -> continue state{data{currentStage = FollowingRide}}
      _ -> do
        _ <- pure $ clearAudioPlayer ""
        if isMockDrill state 
          then void $ pure $ removeSOSAlarmStatus "mock_drill"
          else pure unit
        let newState = state { data { emergencyAudioStatus = COMPLETED },props{ startMapAnimation = false} }
        updateAndExit newState $ Exit newState
  UpdatePeekHeight -> continue state { data { counter = state.data.counter + 1 } }
  UpdateCurrentStage stage -> continue state{data{currentStage = stage}}
  NotificationListener notification -> 
    case notification of
      "SOS_TRIGGERED" -> exit $ RestartTracking state{props{isMock = false}}
      "SOS_RESOLVED" -> do
        _ <- pure $ clearAudioPlayer ""
        let isMock = state.data.currentStage == MockFollowRide
        if isMock then  
            continueWithCmd state{data{sosStatus = Just MockResolved, emergencyAudioStatus = COMPLETED}, props{isMock = isMock}}[ do
              void $ removeSOSRipples ""
              void $ addAndUpdateRideSafeOverlay $ getPoint mockDriverLocation
              pure NoAction
            ]
          else exit $ RestartTracking state{data{emergencyAudioStatus = COMPLETED}, props{isMock = isMock}}
      "SOS_MOCK_DRILL" -> startAudioPlayerCmd [do
          push <- getPushFn Nothing "FollowRideScreen"
          void $ launchAff $ flowRunner defaultGlobalState $ do
            liftFlow $ addAndUpdateSOSRipples $ getPoint mockDriverLocation
            liftFlow $ push $ UpdateMockSOSStatus $ Just MockPending
          pure NoAction
        ] state{props{isMock = true}}
      _ -> continue state
  StopAudioPlayer -> do
    _ <- pure $ clearAudioPlayer ""
    continue state { data { emergencyAudioStatus = COMPLETED } }
  OnNavigate -> exit $ OpenNavigation state
  OnAudioCompleted _ ->
    if (elem state.data.sosStatus [Just Pending, Just MockPending]) && (state.data.emergencyAudioStatus /= COMPLETED)
      then startAudioPlayerCmd [] state { data { emergencyAudioStatus = RESTARTED } }
      else continueWithCmd state [ pure StopAudioPlayer]
  UpdateRoute route -> continue state { data { route = Just route } }
  CallPolice -> do
    void $ pure $ performHapticFeedback unit
    pure $ showDialer "112" false
    continue state
  PersonSelected selectedFollower -> do
    let
      newState =
        state
          { data
            { currentFollower = Just selectedFollower
            , currentStage = FollowingRide
            }
          }
    exit $ RestartTracking newState
  UpdateMockData mockData -> do
    let newState =
          state
            { data
              { driverInfoCardState = Just mockData
              }
            }
    continue newState
  UpdateMockSOSStatus status -> do
    let newState =
          state
            { data
              { sosStatus = status
              }
            }
    if status == Just MockPending 
      then startAudioPlayerCmd [] newState
      else continueWithCmd newState [ pure StopAudioPlayer]
  UpdateStatus (RideBookingRes resp) -> do
    let
      driverInfoCardState = getDriverInfo Nothing (RideBookingRes resp) false

      newState =
        state
          { data
            { driverInfoCardState = Just driverInfoCardState
            , zoneType = getSpecialTag resp.specialLocationTag
            , sosStatus = resp.sosStatus
            }
          , props
            { isRideStarted = driverInfoCardState.status == "INPROGRESS"
            }
          }
    if driverInfoCardState.status == "CANCELLED" then do
      void $ pure $ toast $ getString RIDE_CANCELLED
      exit $ Exit state
    else if isNothing state.data.driverInfoCardState || (fromMaybe mockDriverInfo state.data.driverInfoCardState).status /= driverInfoCardState.status
      then exit $ RestartTracking newState
      else 
        if resp.status == "COMPLETED" 
          then continueWithCmd newState{data{currentStage = RideCompletedStage}} [do
            _ <- runEffectFn1 updateIdMap "FollowsRide"
            pure NoAction
          ]
          else if fromMaybe NotResolved resp.sosStatus == Pending 
            then startAudioPlayerCmd [] newState
            else continue newState
  GenericHeaderAC act -> continueWithCmd state [ pure BackPressed ]
  LoadMessages -> do
    let
      allMessages = getChatMessages FunctionCall
    case (last allMessages) of
      Just value ->
        if STR.null value.message then
          continue state { data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1) }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else if value.sentBy == getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys then
          updateMessagesWithCmd state { data { messages = allMessages, chatSuggestionsList = getSuggestionsfromKey emChatSuggestion "31e3bbf96e4b4208f1328f5b0da57d2e", lastMessage = value, lastSentMessage = value }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else do
          let
            readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))

            unReadMessages = if readMessages == 0 && state.data.currentStage /= ST.ChatWithEM then true else (readMessages < (length allMessages) && state.data.currentStage /= ST.ChatWithEM)

            suggestions = getSuggestionsfromKey emChatSuggestion value.message

            isChatNotificationDismissed = not state.props.isChatNotificationDismissed || state.data.lastMessage.message /= value.message

            showNotification = isChatNotificationDismissed && unReadMessages
          updateMessagesWithCmd state { data { messages = allMessages, chatSuggestionsList = suggestions, lastMessage = value, lastSentMessage = MessagingView.dummyChatComponent, lastReceivedMessage = value }, props { unReadMessages = unReadMessages, showChatNotification = showNotification, canSendSuggestion = true, isChatNotificationDismissed = false, removeNotification = not showNotification, enableChatWidget = showNotification } }
      Nothing -> continue state { props { canSendSuggestion = true } }
  UpdateMessages message sender timeStamp size -> do
    if not state.props.chatCallbackInitiated then
      continue state
    else
      continueWithCmd state { data { messagesSize = size } }
        [ do
            pure $ LoadMessages
        ]
  OpenChatScreen ->
    if not state.props.chatCallbackInitiated then
      continue state
    else
      continueWithCmd state { props { openChatScreen = false } }
        [ do
            pure MessageEmergencyContact
        ]
  MessageEmergencyContact -> do
    case state.data.currentFollower of
        Nothing -> continue state
        Just follower -> 
          if state.data.config.feature.enableChat && follower.priority == 0 && state.props.isRideStarted && not state.props.currentUserOnRide then do
            if not state.props.chatCallbackInitiated then continue state else do
              _ <- pure $ performHapticFeedback unit
              _ <- pure $ setValueToLocalStore READ_MESSAGES (show (length state.data.messages))
              let allMessages = getChatMessages FunctionCall
              continueWithCmd state {data{messages = allMessages, currentStage = ST.ChatWithEM}, props {sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatNotificationDismissed = false,sheetState = Just COLLAPSED}} [do pure $ ResetSheetState]
          else 
            continueWithCmd state
              [ do
                  pure $ MessagingViewAC (MessagingView.Call)
              ]
  ScrollToBottom -> do
    _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
    continue state
  InitializeChat -> continue state { props { chatCallbackInitiated = true } }
  RemoveChat ->
    continueWithCmd state { props { chatCallbackInitiated = false } }
      [ do
          _ <- stopChatListenerService
          _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
          pure $ NoAction
      ]
  MessageExpiryTimer seconds status timerID -> do
    let newState = state{data{counter = state.data.counter + 1}}
    if status == "EXPIRED"
      then do
        _ <- pure $ clearTimerWithId timerID
        if state.data.lastMessage.sentBy == (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) then
        continueWithCmd newState [ do
          pure $ RemoveNotification
        ]
        else continue newState
    else
        continue newState
  SendQuickMessage chatSuggestion -> do
    if state.props.canSendSuggestion then do
      _ <- pure $ sendMessage chatSuggestion
      let
        _ = unsafePerformEffect $ logEvent state.data.logField $ "ny_" <> STR.toLower (STR.replaceAll (STR.Pattern "'") (STR.Replacement "") (STR.replaceAll (STR.Pattern ",") (STR.Replacement "") (STR.replaceAll (STR.Pattern " ") (STR.Replacement "_") chatSuggestion)))
      continue state { props { unReadMessages = false } }
    else
      continue state
  RemoveNotification -> continue state { props { showChatNotification = false, isChatNotificationDismissed = true } }
  CollapseBottomSheet -> continue state { props { sheetState = Just COLLAPSED } }
  NotificationAnimationEnd -> do
    let
      isExpanded = state.props.showChatNotification && state.props.chatCallbackInitiated

      areMessagesEmpty = (length (getChatMessages FunctionCall) == 0)

      showNotification = (areMessagesEmpty || state.props.showChatNotification) && not state.props.isChatNotificationDismissed
    continue state { props { isNotificationExpanded = isExpanded, showChatNotification = showNotification, removeNotification = not showNotification, enableChatWidget = (isExpanded || areMessagesEmpty) && not state.props.isChatNotificationDismissed } }
  MessageViewAnimationEnd -> continue state { props { removeNotification = not state.props.showChatNotification } }
  MessagingViewAC act -> case act of
    MessagingView.TextChanged value -> continue state { data { messageToBeSent = (STR.trim value) }, props { sendMessageActive = (STR.length (STR.trim value)) >= 1 } }
    MessagingView.Call -> do
      case state.data.currentFollower of
        Nothing -> continue state
        Just follower -> do
          void $ pure $ performHapticFeedback unit
          void $ pure $ hideKeyboardOnNavigation true
          pure $ showDialer follower.mobileNumber true
          continue state
    MessagingView.SendMessage -> do
      if state.data.messageToBeSent /= "" then do
        pure $ sendMessage state.data.messageToBeSent
        pure $ setText (getNewIDWithTag "ChatInputEditText") ""
        continue state { data { messageToBeSent = "" }, props { sendMessageActive = false } }
      else
        continue state
    MessagingView.BackPressed -> do
      _ <- pure $ performHapticFeedback unit
      _ <- pure $ hideKeyboardOnNavigation true
      continueWithCmd state
        [ do
            pure $ BackPressed
        ]
    MessagingView.SendSuggestion chatSuggestion ->
      if state.props.canSendSuggestion then do
        _ <- pure $ sendMessage chatSuggestion
        continue state { data { chatSuggestionsList = [] }, props { canSendSuggestion = false } }
      else
        continue state
    _ -> continue state
  PrimaryButtonAC act -> do
    case act of
      PrimaryButton.OnClick -> exit $ Exit state
      _ -> continue state
  ResetSheetState -> continue state { props { sheetState = Nothing } }
  _ -> continue state
  
updateMessagesWithCmd :: FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
updateMessagesWithCmd state =
  continueWithCmd state
    [ do
        if (state.data.currentStage == ST.ChatWithEM) then do
          _ <- pure $ setValueToLocalStore READ_MESSAGES (show (length state.data.messages))
          pure unit
        else
          pure unit
        pure NoAction
    ]

startAudioPlayerCmd :: Array (Effect Action) -> FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
startAudioPlayerCmd array state = do
  let canStartAudio = canStartAudioPlayer state
  continueWithCmd state { data { emergencyAudioStatus = if canStartAudio then STARTED else state.data.emergencyAudioStatus } }
        ([ do
            when canStartAudio
              $ do
                  push <- getPushFn Nothing "FollowRideScreen"
                  void $ pure $ runFn3 startAudioPlayer "ny_ic_sos_danger_full" push OnAudioCompleted
            pure NoAction
        ] <> array)
canStartAudioPlayer :: FollowRideScreenState -> Boolean
canStartAudioPlayer state = 
  let defaultFollower = if isMockDrill state then mockFollower else dummyFollower
      currentFollower = fromMaybe defaultFollower state.data.currentFollower
      status = state.data.emergencyAudioStatus
  in (status == STOPPED && checkCurrentFollower currentFollower) || (status == RESTARTED)

checkCurrentFollower :: ST.Followers -> Boolean
checkCurrentFollower follower = do
  let alarmStatus = getSosAlarmStatus sosAlarmStatus
      exist = elem follower.bookingId alarmStatus
  if exist 
    then false
    else
      let _ = setSosAlarmStatus (alarmStatus <> [follower.bookingId])
      in true


isMockDrill :: FollowRideScreenState -> Boolean
isMockDrill state = state.data.currentStage == MockFollowRide || elem state.data.sosStatus [Just MockPending, Just MockResolved]

getPoint :: GetDriverLocationResp -> Paths
getPoint (GetDriverLocationResp resp) = { lat: resp ^. _lat, lng: resp ^. _lon }

localDelay :: Number -> Flow GlobalState Unit
localDelay seconds = void $ delay $ Milliseconds seconds