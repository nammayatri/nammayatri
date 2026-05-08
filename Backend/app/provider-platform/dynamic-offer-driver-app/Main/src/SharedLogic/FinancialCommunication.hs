module SharedLogic.FinancialCommunication
  ( sendFinancialNotification,
    resolveFleetOwner,
    FinancialEventType (..),
  )
where

import qualified Data.Text.Encoding as TEnc
import Domain.Action.Dashboard.Management.Communication (CommunicationDeliveryDispatchPayload (..))
import qualified Domain.Types.Communication as DComm
import qualified Domain.Types.CommunicationDelivery as DDelivery
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Communication as QComm
import qualified Storage.Queries.CommunicationDelivery as QDelivery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import Tools.Error
import qualified Tools.Notifications as Notify

-- ---------------------------------------------------------------------------
-- PN Throttle — Redis key helpers
--
-- Two counters per (eventType, ownerType, ownerId) per 24-hour window:
--   FinancialPN:Trigger:<eventType>:<DRIVER|FLEET>:<ownerId>
--     → incremented on every end-ride trigger; controls minimum threshold
--   FinancialPN:Sent:<eventType>:<DRIVER|FLEET>:<ownerId>
--     → incremented only when a PN is actually dispatched; caps daily sends
--
-- Both keys are created with a 86400-second TTL on first write (same
-- pattern as SendPaymentLink:RateLimit / DriverFee.incrWithExpiry).
-- ---------------------------------------------------------------------------

finPnTtlSeconds :: Int
finPnTtlSeconds = 86400 -- 24 hours

mkFinPnTriggerKey :: FinancialEventType -> Text -> Text -> Text
mkFinPnTriggerKey eventType ownerType ownerId =
  "FinancialPN:Trigger:" <> show eventType <> ":" <> ownerType <> ":" <> ownerId

mkFinPnSentKey :: FinancialEventType -> Text -> Text -> Text
mkFinPnSentKey eventType ownerType ownerId =
  "FinancialPN:Sent:" <> show eventType <> ":" <> ownerType <> ":" <> ownerId

-- | Atomically increment a Redis counter; set TTL on first creation.
-- Mirrors the incrWithExpiry helper in SharedLogic.Allocator.Jobs.DriverFeeUpdates.DriverFee.
finPnIncrWithExpiry :: (MonadFlow m, CacheFlow m r) => Text -> Int -> m Integer
finPnIncrWithExpiry key expirySeconds = do
  count <- Hedis.incr key
  ttl <- Hedis.ttl key
  when (ttl == -1) $ Hedis.expire key expirySeconds
  pure count

-- | Decide whether to send a financial PN and advance the sent counter.
--
-- Returns True (and increments the sent counter) when BOTH of:
--   triggerCount >= minTriggerThreshold  (enough events happened today)
--   newSentCount  <= maxDailyCount       (cap not yet reached)
--
-- The atomic INCR-then-compare on the sent key means concurrent fibers
-- naturally respect maxDailyCount without a distributed lock.
checkAndTickFinancialPnQuota ::
  (MonadFlow m, CacheFlow m r) =>
  FinancialEventType ->
  Text -> -- "DRIVER" or "FLEET"
  Text -> -- ownerId
  Int ->  -- minTriggerThreshold
  Int ->  -- maxDailyCount
  m Bool
checkAndTickFinancialPnQuota eventType ownerType ownerId minTriggers maxSent = do
  let triggerKey = mkFinPnTriggerKey eventType ownerType ownerId
      sentKey = mkFinPnSentKey eventType ownerType ownerId
  triggerCount <- fromIntegral <$> finPnIncrWithExpiry triggerKey finPnTtlSeconds
  if triggerCount >= minTriggers
    then do
      newSentCount <- fromIntegral <$> finPnIncrWithExpiry sentKey finPnTtlSeconds
      pure $ newSentCount <= maxSent
    else pure False

isQuotaControlledFinancialEvent :: FinancialEventType -> Bool
isQuotaControlledFinancialEvent = \case
  FE_SUBSCRIPTION_LOW_WARNING -> True
  FE_SUBSCRIPTION_BLOCKING -> True
  FE_EARNINGS_LOW_WARNING -> True
  FE_EARNINGS_BLOCKING -> True
  FE_RIDE_ALLOCATION_BLOCKED -> True
  -- Payout + recharge are not throttled (they should be delivered deterministically)
  FE_PAYOUT_INITIATED -> False
  FE_PAYOUT_SUCCESS -> False
  FE_PAYOUT_FAILED -> False
  FE_SUBSCRIPTION_RECHARGE -> False

-- ---------------------------------------------------------------------------
-- Communication helpers
-- ---------------------------------------------------------------------------

mkDriverFacingBodyForFleetLinked :: FinancialEventType -> Text -> Text
mkDriverFacingBodyForFleetLinked eventType body =
  -- Fleet-linked drivers can't recharge/resolve themselves; keep message informative and actionable.
  case eventType of
    FE_SUBSCRIPTION_RECHARGE -> body
    FE_PAYOUT_INITIATED -> body
    FE_PAYOUT_SUCCESS -> body
    FE_PAYOUT_FAILED -> body <> " Please contact your fleet owner to resolve."
    FE_SUBSCRIPTION_LOW_WARNING -> body <> " Please contact your fleet owner to recharge."
    FE_SUBSCRIPTION_BLOCKING -> body <> " Please contact your fleet owner to recharge."
    FE_EARNINGS_LOW_WARNING -> body <> " Please contact your fleet owner."
    FE_EARNINGS_BLOCKING -> body <> " Please contact your fleet owner."
    FE_RIDE_ALLOCATION_BLOCKED -> body <> " Please contact your fleet owner."

createCommunicationAndDeliveries ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasKafkaProducer r,
    HasField "fleetCommunicationDispatchTopic" r Text
  ) =>
  Kernel.Types.Id.Id DM.Merchant ->
  Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
  -- | senderId + role
  (Kernel.Types.Id.Id DP.Person, DComm.CommunicationSenderRole) ->
  -- | communication title/body
  (Text, Text) ->
  -- | recipients: (recipientIdText, role, channels)
  [(Text, DDelivery.CommunicationRecipientRole, [DComm.ChannelType])] ->
  -- | fleetOwnerId for delivery rows (if any)
  Maybe Text ->
  m ()
createCommunicationAndDeliveries merchantId merchantOpCityId (senderId, senderRole) (title, body) recipients mbFleetOwnerIdText = do
  commId <- generateGUID
  now <- getCurrentTime
  let comm =
        DComm.Communication
          { id = commId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            domain = DComm.CD_FINANCIAL,
            senderId = senderId,
            senderRole = senderRole,
            senderDisplayName = Nothing,
            title = title,
            body = body,
            htmlBody = Nothing,
            contentType = DComm.CT_TEXT,
            mediaUrls = Nothing,
            channels = [DComm.CH_PUSH, DComm.CH_WEB],
            ctaButton = Nothing,
            variables = Nothing,
            triggerType = DComm.TT_SYSTEM,
            scheduledAt = Nothing,
            status = DComm.ST_SENDING,
            templateId = Nothing,
            templateName = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QComm.create comm

  topic <- asks (.fleetCommunicationDispatchTopic)
  failureFlags <-
    forM recipients $ \(recipientIdText, recipientRole, channels) ->
      forM channels $ \channel -> do
        deliveryId <- generateGUID
        let delivery =
              DDelivery.CommunicationDelivery
                { id = deliveryId,
                  communicationId = comm.id,
                  merchantId = merchantId,
                  merchantOperatingCityId = merchantOpCityId,
                  recipientId = Kernel.Types.Id.Id recipientIdText,
                  recipientRole = recipientRole,
                  channel = channel,
                  status = DDelivery.DS_PENDING,
                  failureReason = Nothing,
                  fleetOwnerId = mbFleetOwnerIdText,
                  operatorId = Nothing,
                  deliveredAt = Nothing,
                  readAt = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
        QDelivery.create delivery
        if channel == DComm.CH_WEB
          then do
            QDelivery.updateStatusById DDelivery.DS_DELIVERED deliveryId
            pure False
          else do
            let payload =
                  CommunicationDeliveryDispatchPayload
                    { deliveryId = deliveryId.getId,
                      communicationId = comm.id.getId,
                      channel = channel,
                      recipientId = recipientIdText,
                      merchantId = merchantId.getId,
                      merchantOperatingCityId = merchantOpCityId.getId,
                      title = comm.title,
                      body = comm.body,
                      htmlBody = comm.htmlBody,
                      templateId = comm.templateId,
                      templateName = comm.templateName
                    }
            res <- withTryCatch "financialCommunication:produceMessage" $
              produceMessage (topic, Just $ TEnc.encodeUtf8 deliveryId.getId) payload
            case res of
              Right _ -> do
                QDelivery.updateStatusById DDelivery.DS_SENT deliveryId
                pure False
              Left err -> do
                let reason = Just $ "Kafka dispatch failed: " <> show err
                QDelivery.updateFailureReasonAndStatusById deliveryId DDelivery.DS_FAILED reason
                pure True

  let hadFailures = or (concat failureFlags)
  QComm.updateStatusById (if hadFailures then DComm.ST_FAILED else DComm.ST_SENT) commId

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

data FinancialEventType
  = FE_PAYOUT_INITIATED
  | FE_PAYOUT_SUCCESS
  | FE_PAYOUT_FAILED
  | FE_SUBSCRIPTION_RECHARGE
  | FE_SUBSCRIPTION_LOW_WARNING
  | FE_SUBSCRIPTION_BLOCKING
  | FE_EARNINGS_LOW_WARNING
  | FE_EARNINGS_BLOCKING
  | FE_RIDE_ALLOCATION_BLOCKED
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

resolveFleetOwner ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id DP.Person ->
  m (Maybe Text)
resolveFleetOwner driverId = do
  mbAssoc <- QFDA.findByDriverId driverId True
  pure $ (.fleetOwnerId) <$> mbAssoc

-- ---------------------------------------------------------------------------
-- Main entry point
-- ---------------------------------------------------------------------------

-- | Send a financial notification, subject to:
--   1. enableFinancialNotifications flag (global kill-switch in TransporterConfig)
--   2. Daily trigger-threshold + send-cap (Redis counters, daily TTL)
--
-- Owner semantics for throttling:
--   • mbFleetOwnerId = Nothing  → individual driver; throttle per driver
--   • mbFleetOwnerId = Just fid → fleet; throttle per fleet owner
--     All fleet-linked driver events accumulate into the SAME fleet counter,
--     so the fleet owner receives comms only after N cumulative events across
--     all her drivers (not per-driver).  The body sent to the fleet owner
--     contains no driver-specific information.
sendFinancialNotification ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Hedis.HedisLTSFlowEnv r,
    HasKafkaProducer r,
    HasField "fleetCommunicationDispatchTopic" r Text
  ) =>
  Kernel.Types.Id.Id DM.Merchant ->
  Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
  DP.Person ->
  Maybe Text ->
  FinancialEventType ->
  Text ->
  Text ->
  Maybe Text ->
  m ()
sendFinancialNotification merchantId merchantOpCityId driver mbFleetOwnerId eventType title body _mbCorrectiveAction = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let enabled = fromMaybe False transporterConfig.enableFinancialNotifications
  when enabled $ do
    let subscConfig = transporterConfig.subscriptionConfig
        minTriggers = fromMaybe 3 subscConfig.finPnMinTriggerThreshold
        maxDailySent = fromMaybe 2 subscConfig.finPnMaxDailyCount
        -- Throttle at FLEET level for fleet-linked drivers; DRIVER level otherwise.
        (ownerType, ownerId) = case mbFleetOwnerId of
          Nothing -> ("DRIVER", driver.id.getId)
          Just fid -> ("FLEET", fid)

    shouldSend <-
      if isQuotaControlledFinancialEvent eventType
        then checkAndTickFinancialPnQuota eventType ownerType ownerId minTriggers maxDailySent
        else pure True

    if shouldSend
      then do
        logInfo $
          "Sending financial notification: "
            <> show eventType
            <> " ownerType="
            <> ownerType
            <> " ownerId="
            <> ownerId
        case mbFleetOwnerId of
          Nothing ->
            -- Individual driver: direct FCM push, no Communication/delivery rows needed
            Notify.sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.NEW_MESSAGE title body driver driver.deviceToken
          Just fleetOwnerId -> do
            -- Fleet-linked:
            -- - Driver receives PUSH (Kafka→consumer→FCM) + WEB, with driver-friendly wording.
            -- - Fleet owner receives WEB inbox entry with the original (fleet-level) wording.
            -- Body to fleet owner intentionally has no driver-specific information.
            let driverBody = mkDriverFacingBodyForFleetLinked eventType body
                sender = (Kernel.Types.Id.Id fleetOwnerId, DComm.SR_FLEET_OWNER)
                fleetOwnerIdText = Just fleetOwnerId

            createCommunicationAndDeliveries
              merchantId
              merchantOpCityId
              sender
              (title, driverBody)
              [(driver.id.getId, DDelivery.RR_DRIVER, [DComm.CH_PUSH, DComm.CH_WEB])]
              fleetOwnerIdText

            createCommunicationAndDeliveries
              merchantId
              merchantOpCityId
              sender
              (title, body)
              [(fleetOwnerId, DDelivery.RR_FLEET_OWNER, [DComm.CH_WEB])]
              fleetOwnerIdText
      else
        logDebug $
          "Financial notification suppressed by quota: "
            <> show eventType
            <> " ownerType="
            <> ownerType
            <> " ownerId="
            <> ownerId
            <> " (minTriggers="
            <> show minTriggers
            <> " maxDailySent="
            <> show maxDailySent
            <> ")"
