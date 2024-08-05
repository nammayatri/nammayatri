module Components.MessagingView.Common.Types where

import Prelude

import Common.Types.App (ChatComponent)
import MerchantConfig.Types (AppConfig)
import Screens.Types (SearchResultType, Stage, FareProductType)
import Components.ChatView.Controller (ChatComponentConfig)
import Components.MessagingView.Controller 


type MessageNotificationView a = {
  showChatNotification :: Boolean
, enableChatWidget :: Boolean
, isNotificationExpanded :: Boolean
, fareProductType :: FareProductType
, isSpecialZone :: Boolean
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
, removeNotification :: Boolean
, currentStage :: Stage
, user :: UserConfig
, suggestionKey :: String
}