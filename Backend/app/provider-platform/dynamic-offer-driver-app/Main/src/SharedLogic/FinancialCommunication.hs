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

sendFinancialNotification ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
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
    logInfo $ "Sending financial notification: " <> show eventType <> " for driver " <> driver.id.getId
    case mbFleetOwnerId of
      Nothing ->
        -- Individual driver: direct FCM push, no Communication/delivery rows needed
        Notify.sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.NEW_MESSAGE title body driver driver.deviceToken
      Just fleetOwnerId -> do
        -- Fleet-linked:
        -- - Driver receives PUSH (Kafka→consumer→FCM) + WEB, with driver-friendly wording (contact fleet owner).
        -- - Fleet owner receives WEB inbox entry with original wording (recharge/resolve).
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
