-- | Email delivery events from the providers.
--
--   SES (production, AWS): a configuration set publishes Delivery / Bounce / Reject / DeliveryDelay / Complaint events
--   to an SNS topic, whose HTTPS subscription posts here. Every SNS message is signature-checked against the AWS
--   signing certificate, and its topic must be the one configured for the delivery's city.
--
--   SendGrid (sandbox, GCP): the event webhook posts a JSON array here with the city's webhook token in the URL.
module Domain.Action.Internal.EmailEvents
  ( sesEvents,
    sendGridEvents,
  )
where

import Control.Applicative ((<|>))
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.Store.X509 as CryptoStore
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parseMaybe, (.:), (.:?))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.X509 (PubKey (PubKeyRSA), certPubKey, getCertificate)
import qualified Domain.Types.TDSDistributionRecord as DTR
import Environment
import qualified IssueManagement.Utils.RemoteFile as RemoteFile
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Utils.Common
import Network.URI (URI (..), URIAuth (..), parseURI)
import SharedLogic.EmailDeliveryEvents

-- * SES via SNS

data SnsMessage = SnsMessage
  { messageType :: Text,
    messageId :: Text,
    topicArn :: Text,
    message :: Text,
    timestamp :: Text,
    signatureVersion :: Text,
    signature :: Text,
    signingCertUrl :: Text,
    subject :: Maybe Text,
    subscribeUrl :: Maybe Text,
    token :: Maybe Text
  }

instance FromJSON SnsMessage where
  parseJSON = A.withObject "SnsMessage" $ \o ->
    SnsMessage
      <$> o .: "Type"
      <*> o .: "MessageId"
      <*> o .: "TopicArn"
      <*> o .: "Message"
      <*> o .: "Timestamp"
      <*> o .: "SignatureVersion"
      <*> o .: "Signature"
      <*> o .: "SigningCertURL"
      <*> o .:? "Subject"
      <*> o .:? "SubscribeURL"
      <*> o .:? "Token"

-- | SNS posts with Content-Type text/plain, so the body arrives as raw text.
sesEvents :: Text -> Flow APISuccess
sesEvents rawBody = do
  sns <- A.eitherDecodeStrict (TE.encodeUtf8 rawBody) & fromEitherM (\err -> InvalidRequest $ "Invalid SNS message: " <> T.pack err)
  verifySnsSignature sns
  case sns.messageType of
    "SubscriptionConfirmation" -> do
      subscribeUrl <- sns.subscribeUrl & fromMaybeM (InvalidRequest "SubscribeURL missing")
      unless (isAwsSnsUrl subscribeUrl) $ throwError (InvalidRequest "Unexpected SubscribeURL host")
      void $ RemoteFile.fetchRemoteFile subscribeUrl 65536
      logInfo $ "Confirmed SNS subscription to " <> sns.topicArn
    "Notification" -> do
      sesEvent <- A.eitherDecodeStrict (TE.encodeUtf8 sns.message) & fromEitherM (\err -> InvalidRequest $ "Invalid SES event: " <> T.pack err)
      whenJust (join $ parseMaybe parseSesEvent sesEvent) $ applyEvent (FromSes sns.topicArn)
    other -> logInfo $ "Ignoring SNS message of type " <> other
  pure Success

-- | Nothing for event types that do not change delivery status (Send, Open, Click, ...).
parseSesEvent :: A.Value -> Parser (Maybe DeliveryEvent)
parseSesEvent = A.withObject "SesEvent" $ \o -> do
  mbEventType <- (<|>) <$> o .:? "eventType" <*> o .:? "notificationType"
  mail <- o .: "mail"
  messageId <- mail .:? "messageId"
  tags :: Maybe (M.Map Text [Text]) <- mail .:? "tags"
  let deliveryId = tags >>= M.lookup "emailDeliveryId" >>= listToMaybe
      event outcome = Just DeliveryEvent {emailDeliveryId = deliveryId, providerMessageId = messageId, outcome}
  case mbEventType :: Maybe Text of
    Just "Delivery" -> pure $ event Delivered
    Just "DeliveryDelay" -> pure $ event Delayed
    Just "Complaint" -> pure $ event Complained
    Just "Reject" -> do
      reject <- o .:? "reject"
      reason <- maybe (pure Nothing) (.:? "reason") reject
      pure $ event (Rejected reason)
    Just "Bounce" -> do
      bounce <- o .: "bounce"
      bounceType <- bounce .:? "bounceType"
      bounceSubType <- bounce .:? "bounceSubType"
      recipients :: Maybe [A.Object] <- bounce .:? "bouncedRecipients"
      diagnostic <- maybe (pure Nothing) (.:? "diagnosticCode") (recipients >>= listToMaybe)
      pure $ event (Bounced (sesBounceReason bounceSubType) bounceType bounceSubType diagnostic)
    _ -> pure Nothing

sesBounceReason :: Maybe Text -> DTR.TDSFailureReason
sesBounceReason = \case
  Just "NoEmail" -> DTR.ADDRESS_NOT_FOUND
  Just "MailboxFull" -> DTR.MAILBOX_FULL
  Just "MessageTooLarge" -> DTR.ATTACHMENT_TOO_LARGE
  Just "Suppressed" -> DTR.SUPPRESSED
  Just "OnAccountSuppressionList" -> DTR.SUPPRESSED
  _ -> DTR.REJECTED

-- | https://docs.aws.amazon.com/sns/latest/dg/sns-verify-signature-of-message.html
verifySnsSignature :: SnsMessage -> Flow ()
verifySnsSignature sns = do
  unless (isAwsSnsUrl sns.signingCertUrl && ".pem" `T.isSuffixOf` sns.signingCertUrl) $
    throwError (AuthBlocked "SNS signing certificate is not hosted by AWS SNS")
  certPem <- getSigningCertificate sns.signingCertUrl
  publicKey <- case certPubKey . getCertificate <$> CryptoStore.readSignedObjectFromMemory certPem of
    [PubKeyRSA key] -> pure key
    _ -> throwError (AuthBlocked "Unreadable SNS signing certificate")
  let signature = B64.decodeLenient (TE.encodeUtf8 sns.signature)
      signed = TE.encodeUtf8 (stringToSign sns)
      valid = case sns.signatureVersion of
        "1" -> RSA.verify (Just Hash.SHA1) publicKey signed signature
        "2" -> RSA.verify (Just Hash.SHA256) publicKey signed signature
        _ -> False
  unless valid $ throwError (AuthBlocked "Invalid SNS message signature")

stringToSign :: SnsMessage -> Text
stringToSign sns = T.concat [key <> "\n" <> value <> "\n" | (key, Just value) <- fields]
  where
    fields = case sns.messageType of
      "Notification" ->
        [ ("Message", Just sns.message),
          ("MessageId", Just sns.messageId),
          ("Subject", sns.subject),
          ("Timestamp", Just sns.timestamp),
          ("TopicArn", Just sns.topicArn),
          ("Type", Just sns.messageType)
        ]
      _ ->
        [ ("Message", Just sns.message),
          ("MessageId", Just sns.messageId),
          ("SubscribeURL", sns.subscribeUrl),
          ("Timestamp", Just sns.timestamp),
          ("Token", sns.token),
          ("TopicArn", Just sns.topicArn),
          ("Type", Just sns.messageType)
        ]

-- | The signing certificate rarely changes; keep it for a day instead of fetching it per message.
getSigningCertificate :: Text -> Flow ByteString
getSigningCertificate url = do
  let cacheKey = "EmailEvents:SnsSigningCert:" <> url
  cached :: Maybe Text <- Redis.safeGet cacheKey
  case cached >>= either (const Nothing) Just . B64.decode . TE.encodeUtf8 of
    Just pem -> pure pem
    Nothing -> do
      remote <- RemoteFile.fetchRemoteFile url 65536 >>= fromMaybeM (AuthBlocked "SNS signing certificate too large")
      Redis.setExp cacheKey (TE.decodeUtf8 $ B64.encode remote.content) 86400
      pure remote.content

-- | https://sns.<region>.amazonaws.com/... (or .amazonaws.com.cn)
isAwsSnsUrl :: Text -> Bool
isAwsSnsUrl url = case parseURI (T.unpack url) of
  Just URI {uriScheme = "https:", uriAuthority = Just URIAuth {uriRegName = host, uriPort = ""}} ->
    let hostText = T.toLower (T.pack host)
     in "sns." `T.isPrefixOf` hostText && any (`T.isSuffixOf` hostText) [".amazonaws.com", ".amazonaws.com.cn"]
  _ -> False

-- * SendGrid

sendGridEvents :: Text -> [A.Value] -> Flow APISuccess
sendGridEvents token events = do
  forM_ (mapMaybe (join . parseMaybe parseSendGridEvent) events) $ applyEvent (FromSendGrid token)
  pure Success

-- | custom_args come back as top-level keys of each event.
parseSendGridEvent :: A.Value -> Parser (Maybe DeliveryEvent)
parseSendGridEvent = A.withObject "SendGridEvent" $ \o -> do
  eventName :: Text <- o .: "event"
  deliveryId <- o .:? "emailDeliveryId"
  sgMessageId :: Maybe Text <- o .:? "sg_message_id"
  status :: Maybe Text <- o .:? "status"
  reason :: Maybe Text <- o .:? "reason"
  bounceKind :: Maybe Text <- o .:? "type"
  -- sg_message_id is the X-Message-Id the send returned, plus a ".filter..." suffix
  let messageId = T.takeWhile (/= '.') <$> sgMessageId
      event outcome = Just DeliveryEvent {emailDeliveryId = deliveryId, providerMessageId = messageId, outcome}
  pure $ case eventName of
    "delivered" -> event Delivered
    "deferred" -> event Delayed
    "spamreport" -> event Complained
    "blocked" -> event (Rejected reason)
    "bounce"
      | bounceKind == Just "blocked" -> event (Rejected reason)
      | otherwise -> event (Bounced (sendGridBounceReason status) bounceKind status reason)
    "dropped"
      | any (\marker -> maybe False (marker `T.isInfixOf`) reason) ["Bounced Address", "Unsubscribed Address", "Spam Reporting Address"] ->
        event (Bounced DTR.SUPPRESSED (Just "dropped") Nothing reason)
      | otherwise -> event (Rejected reason)
    _ -> Nothing

-- | From the SMTP enhanced status code SendGrid reports.
sendGridBounceReason :: Maybe Text -> DTR.TDSFailureReason
sendGridBounceReason = \case
  Just code
    | "5.1." `T.isPrefixOf` code -> DTR.ADDRESS_NOT_FOUND
    | code `elem` ["4.2.2", "5.2.2"] -> DTR.MAILBOX_FULL
    | code == "5.3.4" -> DTR.ATTACHMENT_TOO_LARGE
  _ -> DTR.REJECTED
