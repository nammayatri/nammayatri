{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Shared logic for the notification webhook. All generic construction lives
-- here (channel dispatch, request-object building, template policy). Each app
-- injects only its app-specific queries and send primitives via
-- 'WebhookServiceHandler', keeping the driver/rider action modules thin.
module Lib.CommunicationEngine.Webhook
  ( NotifyChannel (..),
    NotifyProvider (..),
    Contact (..),
    SendNotificationReq (..),
    SmsMsg (..),
    EmailArgs (..),
    RawTemplate (..),
    MerchantMessageTemplate (..),
    TemplateRequirement (..),
    WebhookServiceHandler (..),
    templatePolicy,
    buildTemplates,
    sendNotificationWebhook,
  )
where

import Data.Aeson (Value (Null))
import qualified Data.Text as T
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.SMS as SMS
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.App ()
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, fromMaybeM)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- | Delivery channel. @PUSH@ routes to the provider-agnostic push interface.
data NotifyChannel = SMS | EMAIL | WHATSAPP | PUSH
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

parseNotifyChannel :: Text -> Maybe NotifyChannel
parseNotifyChannel t = case T.toUpper t of
  "SMS" -> Just SMS
  "EMAIL" -> Just EMAIL
  "WHATSAPP" -> Just WHATSAPP
  "PUSH" -> Just PUSH
  _ -> Nothing

instance FromHttpApiData NotifyChannel where
  parseUrlPiece t = maybe (Left $ "Invalid channel: " <> t) Right (parseNotifyChannel t)

instance ToHttpApiData NotifyChannel where
  toUrlPiece = T.pack . show

-- | Provider, reusing the shared-kernel provider enums. Internal only — used to
-- decide the template policy; never surfaced in the API response.
data NotifyProvider
  = SmsProvider SMS.SmsService
  | WhatsappProvider Whatsapp.WhatsappService
  | PushProvider Notification.NotificationService
  deriving (Show, Eq)

-- | Recipient contact resolved by the app (person query + decryption).
data Contact = Contact
  { personId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    phoneNumber :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text
  }

-- | Body of @POST /internal/notification/send@.
data SendNotificationReq = SendNotificationReq
  { personId :: Text,
    channel :: NotifyChannel,
    title :: Text,
    body :: Text,
    messageData :: Maybe Value,
    templateId :: Maybe Text,
    variables :: Maybe [Text],
    sender :: Maybe Text,
    subject :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | SMS payload handed to the app's raw sender (the app applies its own default
-- sender header when 'sender' is 'Nothing').
data SmsMsg = SmsMsg {phoneNumber :: Text, body :: Text, sender :: Maybe Text, templateId :: Text}

-- | Email payload handed to the app's raw sender.
data EmailArgs = EmailArgs {from :: Text, to :: Text, subject :: Text, body :: Text}

-- | A merchant_message row as the app reads it.
data RawTemplate = RawTemplate
  { messageKey :: Text,
    channel :: Maybe NotifyChannel,
    templateId :: Text,
    message :: Text,
    senderHeader :: Maybe Text
  }

-- | List item returned by the templates endpoint.
data MerchantMessageTemplate = MerchantMessageTemplate
  { messageKey :: Text,
    channel :: Maybe NotifyChannel,
    templateId :: Text,
    message :: Text,
    senderHeader :: Maybe Text,
    templateRequired :: Bool
  }
  deriving (Show, Generic, ToJSON, ToSchema)

data TemplateRequirement = Required | NotRequired
  deriving (Show, Eq)

-- | Whether a @(channel, provider)@ pair requires a pre-registered template.
templatePolicy :: NotifyChannel -> NotifyProvider -> TemplateRequirement
templatePolicy channel provider = case channel of
  PUSH -> NotRequired
  EMAIL -> NotRequired
  WHATSAPP -> Required
  SMS -> case provider of
    SmsProvider SMS.TwillioSms -> NotRequired
    SmsProvider SMS.VonageSms -> NotRequired
    _ -> Required

-- | Pure mapping for the templates endpoint: a template is required if ANY of
-- the merchant's configured providers for the channel requires one (the send
-- falls back across the whole priority list).
buildTemplates :: NotifyChannel -> [NotifyProvider] -> [RawTemplate] -> [MerchantMessageTemplate]
buildTemplates channel providers = map toTemplate
  where
    required = any (\p -> templatePolicy channel p == Required) providers
    toTemplate r =
      MerchantMessageTemplate
        { messageKey = r.messageKey,
          channel = r.channel,
          templateId = r.templateId,
          message = r.message,
          senderHeader = r.senderHeader,
          templateRequired = required
        }

-- | Injected app behaviour: person lookup + the four channel send primitives.
-- The lib builds the request objects; the app just resolves config and calls
-- the provider interface.
data WebhookServiceHandler m = WebhookServiceHandler
  { findContact :: Text -> m (Maybe Contact),
    sendPush :: Contact -> Notification.NotificationReq Value () -> m (),
    sendSms :: Contact -> SmsMsg -> m (),
    sendWhatsapp :: Contact -> Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq -> m (),
    sendEmail :: Contact -> EmailArgs -> m ()
  }

mkNotificationReq :: Contact -> Text -> Text -> Value -> Notification.NotificationReq Value ()
mkNotificationReq contact title body messageData =
  Notification.NotificationReq
    { category = Notification.TRIGGER_FCM,
      subCategory = Nothing,
      showNotification = Notification.SHOW,
      messagePriority = Just Notification.HIGH,
      entity = Notification.Entity Notification.Person contact.personId messageData,
      dynamicParams = (),
      body = body,
      title = title,
      auth = Notification.Auth contact.personId contact.deviceToken contact.notificationToken,
      ttl = Nothing,
      sound = Just "default"
    }

mkWhatsappReq :: Text -> Text -> [Maybe Text] -> Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq
mkWhatsappReq phone templateId variables =
  Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq
    { sendTo = phone,
      templateId = templateId,
      variables = variables,
      ctaButtonUrl = Nothing,
      containsUrlButton = Nothing
    }

-- | Send endpoint: resolve the recipient, build the request, dispatch on channel.
sendNotificationWebhook :: MonadFlow m => WebhookServiceHandler m -> SendNotificationReq -> m APISuccess
sendNotificationWebhook h req = do
  contact <- h.findContact req.personId >>= fromMaybeM (PersonNotFound req.personId)
  case req.channel of
    PUSH ->
      h.sendPush contact (mkNotificationReq contact req.title req.body (fromMaybe Null req.messageData))
    SMS -> do
      phone <- fromMaybeM (InvalidRequest "Recipient phone not available") contact.phoneNumber
      h.sendSms contact SmsMsg {phoneNumber = phone, body = req.body, sender = req.sender, templateId = fromMaybe "" req.templateId}
    WHATSAPP -> do
      phone <- fromMaybeM (InvalidRequest "Recipient phone not available") contact.phoneNumber
      h.sendWhatsapp contact (mkWhatsappReq phone (fromMaybe "" req.templateId) (maybe [Just req.title, Just req.body] (map Just) req.variables))
    EMAIL -> do
      toEmail <- fromMaybeM (InvalidRequest "Recipient email not available") contact.email
      from <- fromMaybeM (InvalidRequest "sender (from email) required for EMAIL channel") req.sender
      h.sendEmail contact EmailArgs {from = from, to = toEmail, subject = fromMaybe req.title req.subject, body = req.body}
  pure Success
