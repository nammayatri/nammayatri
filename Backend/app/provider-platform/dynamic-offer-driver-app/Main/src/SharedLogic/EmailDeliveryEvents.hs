-- | Applying email provider events (SES via SNS, SendGrid event webhook) to email_delivery and to the record the
-- email belongs to. Events arrive at least once and in any order, so an event only ever moves a delivery forward.
module SharedLogic.EmailDeliveryEvents
  ( DeliveryEvent (..),
    EventOutcome (..),
    EventSource (..),
    applyEvent,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.EmailDelivery (EmailDelivery (..))
import qualified Domain.Types.EmailDelivery as DED
import qualified Domain.Types.TDSDistributionRecord as DTR
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.EmailDelivery as QEmailDelivery
import qualified Storage.Queries.TDSDistributionRecord as QRecord

-- | Where the event came from; checked against the delivery's city configuration.
data EventSource
  = -- | SNS topic the (signature-verified) message was published to
    FromSes Text
  | -- | Token the SendGrid event webhook URL carries
    FromSendGrid Text

data EventOutcome
  = Delivered
  | Delayed
  | Complained
  | -- | Bounce, with the failure reason for the record and the provider's type / sub-type / diagnostic
    Bounced DTR.TDSFailureReason (Maybe Text) (Maybe Text) (Maybe Text)
  | -- | Rejected before delivery was attempted, with the provider's reason
    Rejected (Maybe Text)

data DeliveryEvent = DeliveryEvent
  { -- | Our email_delivery id, from the "emailDeliveryId" tag / custom_arg
    emailDeliveryId :: Maybe Text,
    providerMessageId :: Maybe Text,
    outcome :: EventOutcome
  }

applyEvent :: EventSource -> DeliveryEvent -> Flow ()
applyEvent source event = do
  mbByTag <- maybe (pure Nothing) (QEmailDelivery.findById . Id) event.emailDeliveryId
  mbDelivery <- case mbByTag of
    Just delivery -> pure (Just delivery)
    Nothing -> maybe (pure Nothing) (QEmailDelivery.findByProviderMessageId . Just) event.providerMessageId
  case mbDelivery of
    -- Mail sent outside the tracked path (OTPs, plasma notifications) has no row; nothing to update.
    Nothing -> logDebug $ "Email event for an untracked message: " <> show event.providerMessageId
    Just delivery -> do
      authorize source delivery
      let newStatus = outcomeStatus event.outcome
      if not (advances delivery.status newStatus)
        then logDebug $ "Ignoring " <> show newStatus <> " for email delivery " <> delivery.id.getId <> " already " <> show delivery.status
        else do
          now <- getCurrentTime
          let updated = case event.outcome of
                Delivered -> delivery {status = newStatus, deliveredAt = Just now, lastEventAt = Just now, updatedAt = now}
                Bounced _ bounceType bounceSubType detail ->
                  delivery {status = newStatus, bounceType = bounceType, bounceSubType = bounceSubType, failureReason = detail <|> delivery.failureReason, lastEventAt = Just now, updatedAt = now}
                Rejected detail -> delivery {status = newStatus, failureReason = detail <|> delivery.failureReason, lastEventAt = Just now, updatedAt = now}
                _ -> delivery {status = newStatus, lastEventAt = Just now, updatedAt = now}
          QEmailDelivery.updateByPrimaryKey updated
          case delivery.ownerType of
            DED.TDS_RECORD -> applyToTdsRecord updated event.outcome now
            DED.COMM_DELIVERY -> pure ()

authorize :: EventSource -> DED.EmailDelivery -> Flow ()
authorize source delivery = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = delivery.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound delivery.merchantOperatingCityId.getId)
  case source of
    FromSes topicArn ->
      unless (transporterConfig.emailEventSnsTopicArn == Just topicArn) $
        throwError (AuthBlocked "Email events from an unexpected SNS topic")
    FromSendGrid token ->
      unless (isJust transporterConfig.emailEventWebhookToken && transporterConfig.emailEventWebhookToken == Just token) $
        throwError (AuthBlocked "Invalid email event webhook token")

outcomeStatus :: EventOutcome -> DED.EmailDeliveryStatus
outcomeStatus = \case
  Delivered -> DED.DELIVERED
  Delayed -> DED.DELAYED
  Complained -> DED.COMPLAINED
  Bounced {} -> DED.BOUNCED
  Rejected _ -> DED.REJECTED

-- | Forward-only: a final state is never replaced, except that a delivered message can later be marked as spam.
advances :: DED.EmailDeliveryStatus -> DED.EmailDeliveryStatus -> Bool
advances current new
  | new == DED.COMPLAINED = current == DED.DELIVERED
  | otherwise = rank new > rank current
  where
    rank = \case
      DED.SENDING -> 0 :: Int
      DED.SENT -> 1
      DED.DELAYED -> 2
      DED.DELIVERED -> 3
      DED.BOUNCED -> 3
      DED.REJECTED -> 3
      DED.FAILED -> 3
      DED.COMPLAINED -> 4

-- | Only the record's latest attempt moves it: a late event for an earlier attempt leaves a newer result alone.
applyToTdsRecord :: DED.EmailDelivery -> EventOutcome -> UTCTime -> Flow ()
applyToTdsRecord delivery outcome now = do
  mbRecord <- QRecord.findById (Id delivery.ownerId)
  whenJust mbRecord $ \record ->
    when (record.latestEmailDeliveryId == Just delivery.id) $
      case outcome of
        Delivered ->
          when (record.status `elem` [DTR.SENDING, DTR.SENT]) $
            QRecord.updateDelivered DTR.DELIVERED (Just now) Nothing record.id
        Bounced reason _ _ _ -> markFailed record reason
        Rejected _ -> markFailed record DTR.REJECTED
        _ -> pure ()
  where
    markFailed record reason =
      when (record.status `elem` [DTR.SENDING, DTR.SENT, DTR.DELIVERED]) $
        QRecord.updateStatusAndFailureReason DTR.FAILED (Just reason) record.id
