{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FollowRideScreen.Controller where

import Data.Array
import Data.Function.Uncurried
import Data.Int
import Data.Maybe
import Effect.Unsafe
import Engineering.Helpers.Commons
import Engineering.Helpers.Suggestions
import JBridge
import Prelude
import PrestoDOM
import Screens.HomeScreen.Transformer
import Screens.Types
import Services.API
import Services.Backend
import Storage
import Common.Types.App (LazyCheck(..), SosStatus(..))
import Components.DriverInfoCard as DriverInfoCard
import Components.GenericHeader as GenericHeader
import Components.MessagingView as MessagingView
import Data.String as STR
import Engineering.Helpers.LogEvent (logEvent)
import Helpers.Utils (performHapticFeedback)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types as ST

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
  | UpdateMockSOSStatus SosStatus

eval :: Action -> FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
eval action state = case action of
  BackPressed -> updateWithCmdAndExit state [pure StopAudioPlayer] $ Exit state
  UpdatePeekHeight -> continue state { data { counter = state.data.counter + 1 } }
  NotificationListener notification -> 
    case notification of
      "SOS_TRIGGERED" -> exit $ RestartTracking state{data{sosStatus = Just Pending}}
      "SOS_RESOLVED" -> updateWithCmdAndExit state [pure StopAudioPlayer] $ RestartTracking state{data{sosStatus = Just Resolved}}
      _ -> continue state
  StopAudioPlayer -> do
    _ <- pure $ clearAudioPlayer ""
    continue state { data { emergencyAudioStatus = COMPLETED } }
  OnNavigate -> exit $ OpenNavigation state
  OnAudioCompleted _ ->
    if ((fromMaybe NotResolved state.data.sosStatus) == Pending) && (state.data.emergencyAudioStatus /= COMPLETED)
      then startAudioPlayerCmd state { data { emergencyAudioStatus = RESTARTED } }
      else continueWithCmd state [ pure StopAudioPlayer]
  DismissOverlay -> continue state { props { isOverlayDimissed = true } }
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
              { sosStatus = Just status
              }
            }
    if status == Pending 
      then startAudioPlayerCmd newState
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
          }
    if isNothing state.data.driverInfoCardState 
      then exit $ RestartTracking newState
      else 
        if fromMaybe NotResolved resp.sosStatus == Pending 
          then startAudioPlayerCmd newState
          else continue newState
  GenericHeaderAC act -> continueWithCmd state [ pure BackPressed ]
  LoadMessages -> do
    let
      allMessages = [] --getChatMessages FunctionCall
    case (last allMessages) of
      Just value ->
        if value.message == "" then
          continue state { data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1) }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else if value.sentBy == "Customer" then
          updateMessagesWithCmd state { data { messages = allMessages, chatSuggestionsList = [], lastMessage = value, lastSentMessage = value }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else do
          let
            readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))

            unReadMessages = if readMessages == 0 && state.data.currentStage /= ST.ChatWithEM then true else (readMessages < (length allMessages) && state.data.currentStage /= ST.ChatWithEM)

            suggestions = getSuggestionsfromKey value.message

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
    -- if state.data.config.feature.enableChat then do
    --   if not state.props.chatCallbackInitiated then continue state else do
    --     _ <- pure $ performHapticFeedback unit
    --     _ <- pure $ setValueToLocalStore READ_MESSAGES (show (length state.data.messages))
    --     let allMessages = getChatMessages FunctionCall
    --     continue state {data{messages = allMessages, currentStage = ST.ChatWithEM}, props {sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatNotificationDismissed = false,sheetState = Just COLLAPSED}} 
    -- else 
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
          pure $ showDialer follower.mobileNumber false
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
        -- void $ pure $ xlogChatSuggestion state chatSuggestion
        continue state { data { chatSuggestionsList = [] }, props { canSendSuggestion = false } }
      else
        continue state
    _ -> continue state
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
        if (state.props.showChatNotification) then
          pure CollapseBottomSheet
        else
          pure NoAction
    ]

startAudioPlayerCmd :: FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
startAudioPlayerCmd state =      
  continueWithCmd state { data { emergencyAudioStatus = if any (_ == state.data.emergencyAudioStatus) [ STOPPED, RESTARTED ] then STARTED else state.data.emergencyAudioStatus } }
        [ do
            when (any (_ == state.data.emergencyAudioStatus) [ STOPPED, RESTARTED ])
              $ do
                  push <- getPushFn Nothing "FollowRideScreen"
                  void $ pure $ runFn3 startAudioPlayer "ny_ic_sos_danger_full" push OnAudioCompleted
            pure NoAction
        ]