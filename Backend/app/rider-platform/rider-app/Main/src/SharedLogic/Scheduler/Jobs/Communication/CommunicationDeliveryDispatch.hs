module SharedLogic.Scheduler.Jobs.Communication.CommunicationDeliveryDispatch
  ( sendCommunicationDelivery,
  )
where

import qualified Lib.Communication.Domain.Types.Communication as DComm
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Communication.Domain.Action.Dispatch (processCommunicationDeliveryJob)
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
  processCommunicationDeliveryJob jobInfo.jobData dispatchForRider
  pure Complete

-- | Rider-app dispatch function. WEB is handled in-process at creation time;
-- PUSH/SMS/WhatsApp stubs are ready to wire up to rider-specific providers.
dispatchForRider ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  CommunicationDeliveryDispatchJobData ->
  m ()
dispatchForRider jobData =
  case jobData.channel of
    DComm.CH_PUSH ->
      logInfo $ "Rider PUSH dispatch for recipient " <> jobData.recipientId <> " — wire up rider FCM here"
    DComm.CH_SMS ->
      logInfo $ "Rider SMS dispatch for recipient " <> jobData.recipientId <> " — wire up rider SMS provider here"
    DComm.CH_EMAIL ->
      logInfo $ "Rider EMAIL dispatch for recipient " <> jobData.recipientId <> " — wire up rider email provider here"
    DComm.CH_WHATSAPP ->
      logInfo $ "Rider WhatsApp dispatch for recipient " <> jobData.recipientId <> " — wire up rider WhatsApp provider here"
    DComm.CH_WEB ->
      pure ()
