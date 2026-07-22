module Domain.Action.Internal.NotificationWebhook
  ( notificationWebhookSend,
    notificationWebhookTemplates,
  )
where

import qualified Control.Exception as E
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Email.Flow as Email
import Environment
import EulerHS.Prelude hiding (id, map)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.CommunicationEngine.Webhook as Webhook
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import qualified Storage.Queries.MerchantMessage as QMM
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import qualified Tools.Whatsapp as Whatsapp

notificationWebhookSend :: Maybe Text -> Webhook.SendNotificationReq -> Flow APISuccess
notificationWebhookSend mbToken req = do
  verifyToken mbToken
  Webhook.sendNotificationWebhook serviceHandler req

notificationWebhookTemplates :: Maybe Text -> Webhook.NotifyChannel -> Id DP.Person -> Flow [Webhook.MerchantMessageTemplate]
notificationWebhookTemplates mbToken channel userId = do
  verifyToken mbToken
  contact <- findContact' userId.getId >>= fromMaybeM (PersonNotFound userId.getId)
  providers <- resolveProviders' channel contact.merchantOperatingCityId
  raws <- listMerchantMessages' channel contact.merchantOperatingCityId
  pure $ Webhook.buildTemplates channel providers raws

verifyToken :: Maybe Text -> Flow ()
verifyToken mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError (AuthBlocked "Invalid internal api key")

serviceHandler :: Webhook.WebhookServiceHandler Flow
serviceHandler =
  Webhook.WebhookServiceHandler
    { findContact = findContact',
      sendPush = sendPush',
      sendSms = sendSms',
      sendWhatsapp = sendWhatsapp',
      sendEmail = sendEmail'
    }

findContact' :: Text -> Flow (Maybe Webhook.Contact)
findContact' pid = do
  mbPerson <- QPerson.findById (Id pid)
  case mbPerson of
    Nothing -> pure Nothing
    Just person -> do
      mbPhone <- mapM decrypt person.mobileNumber
      mbEmail <- mapM decrypt person.email
      pure $
        Just
          Webhook.Contact
            { personId = person.id.getId,
              merchantId = person.merchantId.getId,
              merchantOperatingCityId = person.merchantOperatingCityId.getId,
              phoneNumber = (\n -> fromMaybe "+91" person.mobileCountryCode <> n) <$> mbPhone,
              email = mbEmail,
              deviceToken = person.deviceToken,
              notificationToken = person.notificationToken
            }

resolveProviders' :: Webhook.NotifyChannel -> Text -> Flow [Webhook.NotifyProvider]
resolveProviders' channel opCityId = do
  usageConfig <-
    getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = opCityId}) Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound opCityId)
  pure $ case channel of
    Webhook.SMS -> map Webhook.SmsProvider usageConfig.smsProvidersPriorityList
    Webhook.WHATSAPP -> map Webhook.WhatsappProvider usageConfig.whatsappProvidersPriorityList
    Webhook.PUSH -> [Webhook.PushProvider usageConfig.notifyPerson]
    Webhook.EMAIL -> []

listMerchantMessages' :: Webhook.NotifyChannel -> Text -> Flow [Webhook.RawTemplate]
listMerchantMessages' channel opCityId = do
  let merchantOpCityId = Id opCityId :: Id DMOC.MerchantOperatingCity
  msgs <- QMM.findAllByMerchantOpCityId merchantOpCityId
  pure $ map toRaw msgs
  where
    toRaw m =
      Webhook.RawTemplate
        { messageKey = show m.messageKey,
          channel = Just channel,
          templateId = m.templateId,
          message = m.message,
          senderHeader = m.senderHeader
        }

sendPush' :: Webhook.Contact -> Notification.NotificationReq Value () -> Flow ()
sendPush' contact req =
  Notify.notifyPerson (Id contact.merchantId) (Id contact.merchantOperatingCityId) (Id contact.personId) req Nothing

sendSms' :: Webhook.Contact -> Webhook.SmsMsg -> Flow ()
sendSms' contact msg = do
  smsCfg <- asks (.smsCfg)
  let sender = fromMaybe smsCfg.sender msg.sender
  Sms.sendSMS (Id contact.merchantId) (Id contact.merchantOperatingCityId) (Sms.SendSMSReq msg.body msg.phoneNumber sender msg.templateId Nothing) >>= Sms.checkSmsResult

sendWhatsapp' :: Webhook.Contact -> Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq -> Flow ()
sendWhatsapp' contact req = do
  result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI (Id contact.merchantId) (Id contact.merchantOperatingCityId) req
  when (result._response.status /= "success") $ throwError (InvalidRequest "WhatsApp send failed")

sendEmail' :: Webhook.Contact -> Webhook.EmailArgs -> Flow ()
sendEmail' _ args = do
  emailServiceConfig <- asks (.emailServiceConfig)
  result <- liftIO $ E.try @E.SomeException $ Email.sendPlainEmail emailServiceConfig args.from [args.to] args.subject args.body
  case result of
    Left err -> throwError (InternalError $ "Email send failed: " <> show err)
    Right () -> pure ()
