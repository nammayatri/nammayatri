module SharedLogic.Allocator.Jobs.Communication.CommunicationDeliveryDispatch
  ( sendCommunicationDelivery,
  )
where

import Domain.Action.Dashboard.Management.Communication (dispatchCommunicationDelivery)
import Kernel.External.Encryption (EncFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Lib.Communication.Domain.Action.Dispatch (processCommunicationDeliveryJob)
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
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
  processCommunicationDeliveryJob jobInfo.jobData dispatchCommunicationDelivery
  pure Complete
