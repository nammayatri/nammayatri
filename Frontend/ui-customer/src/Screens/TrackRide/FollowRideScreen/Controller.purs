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
import Data.Array (elem, last, length, filter, delete, notElem, any)
import Data.Function.Uncurried (runFn3, runFn1, runFn4)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText, updateIdMap, flowRunner, liftFlow)
import Engineering.Helpers.Suggestions (getSuggestionsfromKey, emChatSuggestion)
import JBridge (clearAudioPlayer, getChatMessages, hideKeyboardOnNavigation, scrollToEnd, sendMessage, showDialer, startAudioPlayer, stopChatListenerService, getKeyInSharedPrefKeys, getLayoutBounds, setMapPadding)
import Prelude
import PrestoDOM (BottomSheetState(..), Eval, update, continue, continueWithCmd, defaultPerformLog, exit, updateAndExit)
import Screens.HomeScreen.Transformer (getDriverInfo)
import Screens.HomeScreen.ScreenData (dummyDriverInfo)
import Screens.Types (DriverInfoCard, EmAudioPlayStatus(..), FollowRideScreenStage(..), FollowRideScreenState,NotificationBody)
import Services.API (RideBookingRes(..), Route, GetDriverLocationResp(..), MultiChatReq(..), APISuccessResp(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, setValueToLocalStore)
import Common.Types.App (LazyCheck(..), SosStatus(..), Paths)
import Components.DriverInfoCard as DriverInfoCard
import Components.GenericHeader as GenericHeader
import Components.MessagingView as MessagingView
import Data.Lens ((^.))
import Data.String as STR
import Effect.Aff (Milliseconds(..), launchAff)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.RippleCircles (addAndUpdateRideSafeOverlay, addAndUpdateSOSRipples, addNavigateMarker, removeSOSRipples, removeSafeOverlay)
import Helpers.Utils (performHapticFeedback, getDefaultPixelSize)
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
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import Language.Strings (getString)
import Language.Types (STR(..))
import Timers (clearTimerWithId)
import Storage (getValueToLocalStore)
import DecodeUtil (stringifyJSON, decodeForeignAny, parseJSON)
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import Control.Monad.Except.Trans (runExceptT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Control.Transformers.Back.Trans (runBackT)
import Helpers.API as HelpersAPI
import Services.API as API
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Suggestions(getMessageFromKey, chatSuggestion)
import Constants (languageKey)
import Locale.Utils (getLanguageLocale)
import Components.DriverInfoCard.Controller as DriverInfoCardController 
instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = Exit FollowRideScreenState Boolean
  | RestartTracking FollowRideScreenState
  | OpenNavigation FollowRideScreenState
  | GoToDriverProfiles FollowRideScreenState

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
  | CallPolice SafetyActionTileView.Action
  | StopAudioPlayer
  | StartAudioPlayer
  | OnAudioCompleted String
  | NotificationListener String NotificationBody
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
  | CallSafetyTeam SafetyActionTileView.Action
  | DriverInfoCardAction DriverInfoCardController.Action

eval :: Action -> FollowRideScreenState -> Eval Action ScreenOutput FollowRideScreenState
eval action state = case action of
  DriverInfoCardAction DriverInfoCardController.GoToDriverProfile -> exit $ GoToDriverProfiles state 
  BackPressed -> do 
    case state.data.currentStage of
      ChatWithEM -> continueWithCmd state{data{currentStage = FollowingRide}}
        [ do
            void $ launchAff $ flowRunner defaultGlobalState $ do
              liftFlow $ void $ setMapPadding 0 0 0 $ (getPeekHeight state)
            pure NoAction
        ]
      _ -> do
        _ <- pure $ clearAudioPlayer ""
        let newState = state { data { emergencyAudioStatus = COMPLETED },props{ startMapAnimation = false, chatCallbackInitiated = false} }
        updateAndExit newState $ Exit newState false
  DismissOverlay -> do
    if isMockDrill state 
      then do
        void $ pure $ clearAlarmStatus $ fromMaybe mockFollower state.data.currentFollower
        void $ pure $ clearAudioPlayer ""
        void $ pure $ updateMockDrillsDismissed state
        continueWithCmd state{data{sosStatus = Nothing, emergencyAudioStatus = COMPLETED}, props{isMock = false, startMapAnimation = false}}[
          do
            void $ removeSOSRipples ""
            void $ removeSafeOverlay ""
            pure NoAction
        ]
    else continueWithCmd state [ pure BackPressed]
  UpdatePeekHeight -> continue state { data { counter = state.data.counter + 1 } }
  UpdateCurrentStage stage -> continue state{data{currentStage = stage}}
  NotificationListener notification notificationBody -> 
    case notification of
      "SOS_TRIGGERED" -> exit $ RestartTracking state{data{emergencyAudioStatus = STOPPED}, props{isMock = false}}
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
      "SOS_MOCK_DRILL" -> do
        if notElem state.data.sosStatus [Just Pending, Just Resolved]
        then do
          void $ pure $ clearAlarmStatus $ fromMaybe mockFollower state.data.currentFollower
          startAudioPlayerCmd [do
            push <- getPushFn Nothing "FollowRideScreen"
            void $ pure $ deleteDismisedMockDrills state.data.currentFollower
            void $ launchAff $ flowRunner defaultGlobalState $ do
              liftFlow $ addAndUpdateSOSRipples $ getPoint mockDriverLocation
              liftFlow $ push $ UpdateMockSOSStatus $ Just MockPending
            pure NoAction
          ] state{props{isMock = true}, data{emergencyAudioStatus = STOPPED}}
        else continue state
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
  CallPolice SafetyActionTileView.OnClick -> do
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
      driverInfoCardState = getDriverInfo Nothing (RideBookingRes resp) false (fromMaybe dummyDriverInfo (state.data.driverInfoCardState))
      sosStatus = getSosStatus resp.sosStatus resp.id
      newState =
        state
          { data
            { driverInfoCardState = Just driverInfoCardState
            , zoneType = getSpecialTag resp.specialLocationTag
            , sosStatus = sosStatus
            }
          , props
            { isRideStarted = driverInfoCardState.status == "INPROGRESS"
            , isMock = elem sosStatus [Just MockPending, Just MockResolved]
            }
          }
    if driverInfoCardState.status == "CANCELLED" then do
      void $ pure $ EHU.showToast $ getString RIDE_CANCELLED
      exit $ Exit state{props{chatCallbackInitiated = false}} true
    else if isNothing state.data.driverInfoCardState || (fromMaybe mockDriverInfo state.data.driverInfoCardState).status /= driverInfoCardState.status
      then exit $ RestartTracking newState
      else 
        if resp.status == "COMPLETED" 
          then continueWithCmd newState{data{currentStage = RideCompletedStage}} [do
            _ <- runEffectFn1 updateIdMap "FollowsRide"
            pure NoAction
          ]
          else if elem (getSosStatus resp.sosStatus resp.id) [Just Pending, Just MockPending]
            then startAudioPlayerCmd [] newState
            else continue newState
  GenericHeaderAC act -> continueWithCmd state [ pure BackPressed ]
  LoadMessages -> do
    let allMessages = getChatMessages FunctionCall
        toChatComponentConfig { message, sentBy, timeStamp, type: type_, delay } = 
          { message, messageTitle: Nothing, messageAction: Nothing, messageLabel: Nothing, sentBy, timeStamp, type: type_, delay}
        transformedMessages = map toChatComponentConfig allMessages
    case (last allMessages) of
      Just value ->
        if STR.null value.message then
          continue state { data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1) }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else if value.sentBy == getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys then
          updateMessagesWithCmd state { data { messages = transformedMessages, chatSuggestionsList = getSuggestionsfromKey emChatSuggestion "31e3bbf96e4b4208f1328f5b0da57d2e", lastMessage = toChatComponentConfig value, lastSentMessage = value }, props { canSendSuggestion = true, isChatNotificationDismissed = false } }
        else do
          let
            readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))

            unReadMessages = if readMessages == 0 && state.data.currentStage /= ST.ChatWithEM then true else (readMessages < (length transformedMessages) && state.data.currentStage /= ST.ChatWithEM)

            suggestions = getSuggestionsfromKey emChatSuggestion value.message

            isChatNotificationDismissed = not state.props.isChatNotificationDismissed || state.data.lastMessage.message /= value.message

            showNotification = isChatNotificationDismissed && unReadMessages
          updateMessagesWithCmd state { data { messages = transformedMessages, chatSuggestionsList = suggestions, lastMessage = toChatComponentConfig value, lastSentMessage = MessagingView.dummyChatComponent, lastReceivedMessage = value }, props { unReadMessages = unReadMessages, showChatNotification = showNotification, canSendSuggestion = true, isChatNotificationDismissed = false, removeNotification = not showNotification, enableChatWidget = showNotification } }
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
        Just follower -> do
          if state.data.config.feature.enableChat then do
            if not state.props.chatCallbackInitiated then continue state else do
              void $ pure $ performHapticFeedback unit
              _ <- pure $ setValueToLocalStore READ_MESSAGES (show (length state.data.messages))
              let allMessages = getChatMessages FunctionCall
                  toChatComponentConfig { message, sentBy, timeStamp, type: type_, delay } = 
                    { message, messageTitle: Nothing, messageAction: Nothing, messageLabel: Nothing, sentBy, timeStamp, type: type_, delay}
                  transformedMessages = map toChatComponentConfig allMessages
              continueWithCmd state {data{messages = transformedMessages, currentStage = ST.ChatWithEM}, props {sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatNotificationDismissed = false,sheetState = Just COLLAPSED}} [do 
                void $ launchAff $ flowRunner defaultGlobalState $ updateMapPadding state
                pure $ ResetSheetState]
          else 
            continueWithCmd state
              [ do
                  void $ launchAff $ flowRunner defaultGlobalState $ updateMapPadding state
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
      triggerFCM state { props { unReadMessages = false } } $ getMessageFromKey emChatSuggestion chatSuggestion (getLanguageLocale languageKey)
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
        let message = state.data.messageToBeSent
        triggerFCM state { data { messageToBeSent = "" }, props { sendMessageActive = false } } message
      else
        continue state
    MessagingView.BackPressed -> do
      void $ pure $ performHapticFeedback unit
      _ <- pure $ hideKeyboardOnNavigation true
      continueWithCmd state
        [ do
            pure $ BackPressed
        ]
    MessagingView.SendSuggestion chatSuggestion ->
      if state.props.canSendSuggestion then do
        _ <- pure $ sendMessage chatSuggestion
        triggerFCM state { data { chatSuggestionsList = [] }, props { canSendSuggestion = false } } $ getMessageFromKey emChatSuggestion chatSuggestion (getLanguageLocale languageKey)
      else
        continue state
    _ -> continue state
  PrimaryButtonAC act -> do
    case act of
      PrimaryButton.OnClick -> exit $ Exit state{props{chatCallbackInitiated = false}} true
      _ -> continue state
  ResetSheetState -> continue state { props { sheetState = Nothing } }
  CallSafetyTeam SafetyActionTileView.OnClick -> do
    pure $ showDialer state.data.config.safety.safetyTeamNumber false
    continue state
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
                  void $ pure $ runFn4 startAudioPlayer "ny_ic_sos_danger_full" push OnAudioCompleted "0"
            pure NoAction
        ] <> array)
canStartAudioPlayer :: FollowRideScreenState -> Boolean
canStartAudioPlayer state = 
  let defaultFollower = if state.props.isMock then mockFollower else dummyFollower
      currentFollower = fromMaybe defaultFollower state.data.currentFollower
      status = state.data.emergencyAudioStatus
  in (status == STOPPED && (checkCurrentFollower currentFollower state.props.isMock)) || (status == RESTARTED)

getMockSosAlarmStatus :: String -> Array String
getMockSosAlarmStatus key = decodeForeignAny (parseJSON (getKeyInSharedPrefKeys key)) []

checkCurrentFollower :: ST.Followers -> Boolean -> Boolean
checkCurrentFollower follower isMock = do
  let alarmStatus = getSosAlarmStatus sosAlarmStatus
      bookingId = case isMock, follower.bookingId /= "mock_drill" of 
                      true, true -> "mock_" <> follower.bookingId
                      _, _ -> follower.bookingId
      exist = elem bookingId alarmStatus
  if exist 
    then false
    else
      let _ = setSosAlarmStatus (alarmStatus <> [bookingId])
      in true


isMockDrill :: FollowRideScreenState -> Boolean
isMockDrill state = state.data.currentStage == MockFollowRide || elem state.data.sosStatus [Just MockPending, Just MockResolved]

getPoint :: GetDriverLocationResp -> Paths
getPoint (GetDriverLocationResp resp) = { lat: resp ^. _lat, lng: resp ^. _lon }

localDelay :: Number -> Flow GlobalState Unit
localDelay seconds = void $ delay $ Milliseconds seconds

updateMockDrillsDismissed :: FollowRideScreenState -> Effect Unit
updateMockDrillsDismissed state = do
  let mockDrillsDismissed = getValueFromCache "MOCK_SOS_FOR_RIDES" getMockSosAlarmStatus
      currentFollower = fromMaybe mockFollower state.data.currentFollower
  if notElem currentFollower.bookingId mockDrillsDismissed then
    void $ pure $ setValueToCache "MOCK_SOS_FOR_RIDES" (mockDrillsDismissed <> [currentFollower.bookingId]) stringifyJSON
  else pure unit

deleteDismisedMockDrills :: Maybe ST.Followers -> Effect Unit
deleteDismisedMockDrills follower = do
  let mockDrillsDismissed = getValueFromCache "MOCK_SOS_FOR_RIDES" getMockSosAlarmStatus
      currentFollower = fromMaybe mockFollower follower
  if elem currentFollower.bookingId mockDrillsDismissed then
    void $ pure $ setValueToCache "MOCK_SOS_FOR_RIDES" (delete currentFollower.bookingId mockDrillsDismissed) stringifyJSON
  else pure unit
  

updateMapPadding :: FollowRideScreenState -> Flow GlobalState Unit
updateMapPadding state = do
      let 
        srcAndDstBounds = runFn1 getLayoutBounds $ getNewIDWithTag "FollowRideSourceDestinationView"
      liftFlow $ void $ setMapPadding 0 0 0 $ getPeekHeight state + getDefaultPixelSize srcAndDstBounds.height + 150
  
getPeekHeight :: FollowRideScreenState -> Int
getPeekHeight _ =
  let
    headerViewBounds = runFn1 getLayoutBounds $ getNewIDWithTag "FollowRideHeaderView"
    requiredPeekHeight = getDefaultPixelSize $ headerViewBounds.height
  in
    if requiredPeekHeight < 156 then 156 else requiredPeekHeight

getSosStatus :: Maybe SosStatus -> String -> Maybe SosStatus
getSosStatus status id = 
    case status of 
      _ | elem status [Just MockResolved, Just MockPending] -> do
        let mockDrillsDismissed = getValueFromCache "MOCK_SOS_FOR_RIDES" getMockSosAlarmStatus
        if elem id mockDrillsDismissed 
          then Nothing
          else status
      _ -> status
  
clearAlarmStatus :: ST.Followers -> Unit
clearAlarmStatus currentFollower = do
  let bookingId = if currentFollower.bookingId /= "mock_drill" then "mock_" <> currentFollower.bookingId else currentFollower.bookingId
      _ = removeSOSAlarmStatus bookingId
  unit

triggerFCM :: FollowRideScreenState -> String -> Eval Action ScreenOutput FollowRideScreenState
triggerFCM state message = do 
  continueWithCmd state [
        do
          void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
            $ do
                push <- liftFlowBT $ getPushFn Nothing "FollowRideScreen"
                case state.data.currentFollower of
                  Just follower -> 
                    maybe (pure unit) (\personId -> do
                        let requestBody = 
                              { chatPersonId : personId
                              , body : message
                              , title : "Message from " <> 
                                          if any (_ == (getValueToLocalStore USER_NAME)) ["__failed", ""] 
                                            then (getString USER) 
                                            else (getValueToLocalStore USER_NAME)
                              , source : Just API.TRUSTED_CONTACT
                              , channelId : Just $ getChatChannelId state
                              , showNotification : Nothing
                              }
                        (_ :: APISuccessResp) <- HelpersAPI.callApiBT $ MultiChatReq requestBody
                        pure unit) follower.personId
                  Nothing -> pure unit
          pure NoAction
    ]

getChatChannelId :: FollowRideScreenState -> String
getChatChannelId state = do 
  let cFollower = fromMaybe dummyFollower state.data.currentFollower
      customerId = getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys
      rideId = fromMaybe "" $ state.data.driverInfoCardState <#> _.rideId
  if cFollower.priority == 0 then rideId else rideId <> "$" <> customerId