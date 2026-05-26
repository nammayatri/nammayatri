module SharedLogic.Allocator.Jobs.Communication.CommunicationDeliveryDispatch
  ( sendCommunicationDelivery,
  )
where

import Domain.Action.Dashboard.Management.Communication
  ( CommunicationDeliveryDispatchPayload (..),
    processFleetCommunicationDeliveryPayload,
  )
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), CommunicationDeliveryDispatchJobData (..))
import Storage.Beam.Communication ()
import Storage.Beam.CommunicationDelivery ()

sendCommunicationDelivery ::
  ( MonadIO m,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "requestId" r (Maybe Text),
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasKafkaProducer r,
    Hedis.HedisFlow m r,
    Hedis.HedisLTSFlowEnv r
  ) =>
  Job 'CommunicationDeliveryDispatch ->
  m ExecutionResult
sendCommunicationDelivery Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      payload =
        CommunicationDeliveryDispatchPayload
          { deliveryId = jobData.deliveryId,
            communicationId = jobData.communicationId,
            channel = jobData.channel,
            recipientId = jobData.recipientId,
            merchantId = jobData.merchantId,
            merchantOperatingCityId = jobData.merchantOperatingCityId,
            title = jobData.title,
            body = jobData.body,
            htmlBody = jobData.htmlBody,
            templateId = jobData.templateId,
            templateName = jobData.templateName
          }
  processFleetCommunicationDeliveryPayload payload
  pure Complete
