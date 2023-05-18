{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SendMessageJob where

import qualified Data.Map as M
import qualified Domain.Types.Message.Message as Domain
import qualified Domain.Types.Message.MessageReport as Domain
import qualified Domain.Types.Message.MessageTranslation as D
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Commons (KafkaTopic)
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import SharedLogic.Allocator (SchedulerJobType (..), SendDriverMessagesJobData (..))
import qualified Storage.Queries.Message.MessageReport as MRQuery
import qualified Storage.Queries.Message.MessageTranslation as MTQuery

sendMessagesToDrivers ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    Log m,
    HasField "broadcastMessageTopic" r KafkaTopic,
    HasField "kafkaProducerTools" r KafkaProducerTools,
    HasField "sendMessageToDriversBatchSize" r Int,
    HasField "sendMessageToDriversSndsDelay" r Int
  ) =>
  Job 'SendDriverMessages ->
  m ExecutionResult
sendMessagesToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  batchSize <- asks (.sendMessageToDriversBatchSize)
  let (batchIds, restIds) = splitAt batchSize jobData.driversIds
  logDebug $ "DriverId to which the message is sent" <> show batchIds
  mapM_ (addToKafka jobData.message) batchIds
  nextBatchSeconds <- asks (.sendMessageToDriversSndsDelay)
  let nextBatchDelay = secondsToNominalDiffTime $ Seconds nextBatchSeconds
  case restIds of
    [] -> pure Complete
    _ -> do
      Esq.runTransaction $
        createJobIn @_ @'SendDriverMessages
          nextBatchDelay
          1
          (SendDriverMessagesJobData jobData.messageId jobData.message restIds)
      pure Complete
  where
    addToKafka message driverId = do
      topicName <- asks (.broadcastMessageTopic)
      now <- getCurrentTime
      void $ try @_ @SomeException (Esq.runTransaction $ MRQuery.create (mkMessageReport now driverId)) -- avoid extra DB call to check if driverId exists
      msg <- createMessageLanguageDict message
      produceMessage
        (topicName, Just (encodeUtf8 $ getId driverId))
        msg
    mkMessageReport now driverId =
      Domain.MessageReport
        { driverId,
          messageId = Id jobInfo.jobData.messageId,
          deliveryStatus = Domain.Queued,
          readStatus = False,
          likeStatus = False,
          messageDynamicFields = M.empty,
          reply = Nothing,
          createdAt = now,
          updatedAt = now
        }
    createMessageLanguageDict message = do
      translations <- Esq.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

addTranslation :: Domain.RawMessage -> D.MessageTranslation -> (Text, Domain.RawMessage)
addTranslation Domain.RawMessage {..} trans =
  (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})
