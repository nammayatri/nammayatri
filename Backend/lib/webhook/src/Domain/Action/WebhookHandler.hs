module Domain.Action.WebhookHandler where

import Control.Lens ((^?), _head)
import Domain.Action.Flow as WFlow
import qualified Domain.Types.WebhookExtra as WT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Webhook.Storage.Beam.BeamFlow as BFlow
import Lib.Webhook.Storage.Queries.Webhook
import qualified Lib.Webhook.Types.Webhook as DTWeb

data WebhookJobInfo = WebhookJobInfo
  { mode :: WT.WebhookDeliveryType,
    webhookId :: Maybe (Id DTWeb.Webhook),
    statusToCheck :: [WT.WebhookStatus],
    retryCount :: Maybe Int,
    retryLimit :: Int,
    event :: Maybe WT.WebhookEvent,
    limit :: Maybe Int,
    webhookConfig :: WT.ExternalWebhookConfigs,
    nextJobScheduleTimeThreshold :: Int,
    rescheduleTimeThreshold :: Int,
    batchId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FunctionStatus = PROCESS_NEXT_BATCH Int | SCHEDULE_NEXT_JOB Int Int | RETRIES_EXHAUSTED | BATCH_PROCCESSING_COMPLETED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WebhookDataEntity = WebhookDataEntity
  { driver_id :: Text,
    mandate_id :: Text,
    vehicle_number :: Text,
    phone_number :: Text,
    name :: Text,
    org_id :: Text,
    mandate_created_at :: UTCTime,
    order_id :: Text,
    mandate_status :: Text,
    event_name :: Text,
    last_sent_at :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

sendWebhookWithRetry ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    BFlow.BeamFlow m r
  ) =>
  WebhookJobInfo ->
  m FunctionStatus
sendWebhookWithRetry WebhookJobInfo {..} = do
  case mode of
    WT.BATCHING -> do
      let limit' = fromMaybe 0 limit
      webhooksToDeliver <- findAllWithStatusModeWithinRetryThreshold statusToCheck mode retryLimit event Nothing batchId limit'
      case webhooksToDeliver of
        [] -> pure BATCH_PROCCESSING_COMPLETED
        _ -> do
          forM_ webhooksToDeliver $ \webooksData -> do
            void $ processWebhookData webooksData
          pure $ PROCESS_NEXT_BATCH rescheduleTimeThreshold
    WT.REAL_TIME -> do
      let retryCount' = fromMaybe 0 retryCount
          limit' = fromMaybe 0 limit
      case webhookId of
        Just _ -> do
          webhookToDeliver <- findAllWithStatusModeWithinRetryThreshold statusToCheck mode retryCount' event webhookId Nothing limit'
          let mbWebhookData = webhookToDeliver ^? _head
          status <- maybe (pure WT.NO_CONFIG) processWebhookData mbWebhookData
          case status of
            WT.DELIVERED -> pure BATCH_PROCCESSING_COMPLETED
            WT.NO_CONFIG -> pure BATCH_PROCCESSING_COMPLETED
            _ -> do
              if retryCount' >= retryLimit
                then pure RETRIES_EXHAUSTED
                else pure $ SCHEDULE_NEXT_JOB (rescheduleTimeThreshold * (2 ^ retryCount')) (retryCount' + 1)
        Nothing -> pure BATCH_PROCCESSING_COMPLETED
  where
    processWebhookData ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EncFlow m r,
        MonadFlow m,
        BFlow.BeamFlow m r
      ) =>
      DTWeb.Webhook ->
      m WT.WebhookStatus
    processWebhookData webooksData = do
      if (webooksData.retryCount > retryLimit)
        then do
          updateWebhookStatus WT.RETRIES_ENDED webooksData.id
          return WT.RETRIES_ENDED
        else do
          updateRetryCount (webooksData.retryCount + 1) webooksData.id
          let req = buildWebHookRequest webooksData
          resp <- withTryCatch "nyWebhook:webhookHandler" $ WFlow.nyWebhook webhookConfig.baseUrl webhookConfig.password webhookConfig.username req
          status <- do
            case resp of
              Right _ -> do
                updateWebhookStatus WT.DELIVERED webooksData.id
                return WT.DELIVERED
              Left _ -> do
                updateWebhookStatus WT.FAILED webooksData.id
                return WT.FAILED
          return status
    buildWebHookRequest webooksData = do
      WT.ExternalWebhookData
        { id = webooksData.id.getId,
          shortId = webooksData.shortId.getShortId,
          eventName = webooksData.eventName,
          webhookData = webooksData.webhookData
        }

mkWebhookBatchKey :: Text -> Text
mkWebhookBatchKey batchId = "WEBHOOK:JOB:BID:" <> batchId

mkWebhookRealTimeKey :: Text -> Text
mkWebhookRealTimeKey webhookId = "WEBHOOK:JOB:WebhookID:" <> webhookId
