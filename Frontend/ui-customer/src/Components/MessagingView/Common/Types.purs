module Components.MessagingView.Common.Types where

import Prelude

import Common.Types.App (ChatComponent)
import MerchantConfig.Types (AppConfig)
import Screens.Types (SearchResultType)
import Components.ChatView.Controller (ChatComponentConfig)


type MessageNotificationView a = {
  showChatNotification :: Boolean
, enableChatWidget :: Boolean
, isNotificationExpanded :: Boolean
, currentSearchResultType :: SearchResultType
, config :: AppConfig
, rideStarted ::Boolean
, lastMessage :: ChatComponent
, lastReceivedMessage :: ChatComponent
, lastSentMessage :: ChatComponent
, removeNotificationAction :: a
, messageViewAnimationEnd :: a
, messageReceiverAction :: a
, sendQuickMessageAction :: (String -> a)
, timerCounter :: Int
, messageExpiryAction :: (Int ->  String ->  String -> a)
, chatSuggestions :: Array String
, messages :: Array ChatComponentConfig
}