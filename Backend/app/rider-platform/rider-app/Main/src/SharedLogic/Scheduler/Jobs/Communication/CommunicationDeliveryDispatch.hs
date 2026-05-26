module SharedLogic.Scheduler.Jobs.Communication.CommunicationDeliveryDispatch
  ( sendCommunicationDelivery,
  )
where

import qualified Lib.Communication.Domain.Types.Communication as DComm
import qualified Lib.Communication.Domain.Types.CommunicationDelivery as DDelivery
import qualified Lib.Communication.Storage.Queries.CommunicationDelivery as QDelivery
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler (RiderJobType (..), CommunicationDeliveryDispatchJobData (..))
import Storage.Beam.Communication ()
import Storage.Beam.CommunicationDelivery ()

sendCommunicationDelivery ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Job 'CommunicationDeliveryDispatch ->
  m ExecutionResult
sendCommunicationDelivery Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      deliveryId = Kernel.Types.Id.Id jobData.deliveryId
  result <- try @_ @SomeException $ dispatchForRider jobData
  case result of
    Right _ -> QDelivery.updateStatusById DDelivery.DS_SENT deliveryId
    Left err -> do
      logError $ "Rider communication dispatch failed for delivery " <> jobData.deliveryId <> ": " <> show err
      QDelivery.updateStatusById DDelivery.DS_FAILED deliveryId
  pure Complete

dispatchForRider ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CommunicationDeliveryDispatchJobData ->
  m ()
dispatchForRider jobData =
  case jobData.channel of
    DComm.CH_PUSH ->
      logInfo $ "Rider PUSH dispatch for recipient " <> jobData.recipientId <> " — implement via rider FCM"
    DComm.CH_SMS ->
      logInfo $ "Rider SMS dispatch for recipient " <> jobData.recipientId <> " — implement via rider SMS provider"
    DComm.CH_EMAIL ->
      logInfo $ "Rider EMAIL dispatch for recipient " <> jobData.recipientId <> " — implement via rider email provider"
    DComm.CH_WHATSAPP ->
      logInfo $ "Rider WhatsApp dispatch for recipient " <> jobData.recipientId <> " — implement via rider WhatsApp provider"
    DComm.CH_WEB ->
      logInfo $ "Rider WEB delivery for recipient " <> jobData.recipientId <> " — handled in-process"
