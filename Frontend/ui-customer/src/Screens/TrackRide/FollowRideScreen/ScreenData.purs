module Screens.FollowRideScreen.ScreenData where

import ConfigProvider
import Data.Maybe
import Prelude
import Screens.HomeScreen.ScreenData
import Screens.Types
import PrestoDOM (BottomSheetState(..))
import Foreign.Object (empty)

initData :: FollowRideScreenState
initData =
  { data:
      { driverInfoCardState: Nothing
      , currentStage: PersonList
      , currentFollower: Nothing
      , logField: empty
      , followers: []
      , zoneType: dummyZoneType
      , route: Nothing
      , speed: 1
      , config: getAppConfig appConfig
      , messages: []
      , messagesSize: "-1"
      , chatSuggestionsList: []
      , lastMessage: { message: "", sentBy: "", timeStamp: "", type: "", delay: 0 }
      , lastSentMessage: { message: "", sentBy: "", timeStamp: "", type: "", delay: 0 }
      , lastReceivedMessage: { message: "", sentBy: "", timeStamp: "", type: "", delay: 0 }
      , messageToBeSent: ""
      , sosStatus: Nothing
      , emergencyAudioStatus: STOPPED
      , counter: 0
      }
  , props:
      { city: AnyCity
      , showChatNotification: false
      , canSendSuggestion: true
      , isChatNotificationDismissed: false
      , unReadMessages: false
      , removeNotification: true
      , enableChatWidget: false
      , chatCallbackInitiated: false
      , openChatScreen: false
      , sendMessageActive: false
      , sheetState: Nothing
      , currentSheetState: COLLAPSED
      , isNotificationExpanded: false
      , isOverlayDimissed: false
      }
  }

dummyFollower :: Followers
dummyFollower =
  { name: Nothing
  , bookingId: ""
  , mobileNumber: ""
  , priority: 0
  }
