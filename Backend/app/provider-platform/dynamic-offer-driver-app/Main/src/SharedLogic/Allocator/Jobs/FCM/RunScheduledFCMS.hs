module SharedLogic.Allocator.Jobs.FCM.RunScheduledFCMS
  ( runScheduledFCMS,
  )
where

import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.MessageBuilder (addBroadcastMessageToKafka)
import System.Environment (lookupEnv)

lookupBroadcastPush :: IO Bool
lookupBroadcastPush = fromMaybe False . (>>= readMaybe) <$> lookupEnv "BROADCAST_KAFKA_PUSH"

runScheduledFCMS ::
  (HasField "broadcastMessageTopic" r KafkaTopic, HasKafkaProducer r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Job 'ScheduledFCMS ->
  m ExecutionResult
runScheduledFCMS Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  kafkaPush <- L.runIO lookupBroadcastPush
  let jobData = jobInfo.jobData
  fork "Adding messages to kafka queue" $ mapM_ (addBroadcastMessageToKafka kafkaPush jobData.message) jobData.driverIds
  return Complete
