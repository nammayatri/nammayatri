-- | Kafka loop for consumers whose output must not lose or reorder messages
-- (DRIVER_POOL_STATE). Unlike 'Transporter.Kafka.Flow':
--
--   * messages are handled one at a time, in order, and a failing message is
--     retried until it succeeds, so nothing is skipped;
--   * offsets are committed explicitly, only after messages are handled;
--   * a watermark (epoch ms) is reported: the oldest last-handled event time
--     across partitions, or the current time whenever the consumer is caught up.
module Transporter.Kafka.Committing
  ( runOrderedCommitting,
    isRdKafkaError,
  )
where

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kafka.Types (KafkaError)
import Kernel.Prelude
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import qualified Prelude as P

-- | Commit and report the watermark after this many handled messages.
progressEvery :: Int
progressEvery = 100

-- | While idle, commit and report at most this often (ms), instead of on every
-- empty poll.
idleProgressEveryMs :: Integer
idleProgressEveryMs = 5000

runOrderedCommitting ::
  (FromJSON event) =>
  L.FlowRuntime ->
  AppEnv ->
  Consumer.KafkaConsumer ->
  -- | event time, epoch ms
  (event -> Integer) ->
  (event -> Flow ()) ->
  -- | report the watermark, epoch ms
  (Integer -> Flow ()) ->
  IO ()
runOrderedCommitting flowRt appEnv kc eventTime handler reportWatermark = go M.empty 0 0
  where
    flow = runFlowR flowRt appEnv
    go lastByPartition handled lastIdleReport = do
      polled <- Consumer.pollMessage kc (Consumer.Timeout 500)
      case polled of
        Left err | isRdKafkaError "RdKafkaRespErrTimedOut" err -> do
          -- Nothing pending: every partition is caught up.
          nowMs <- round . (* 1000) <$> getPOSIXTime
          if nowMs - lastIdleReport >= idleProgressEveryMs
            then progress nowMs >> go M.empty 0 nowMs
            else go M.empty 0 lastIdleReport
        Left err -> do
          flow $ logError $ "kafka poll error: " <> show err
          go lastByPartition handled lastIdleReport
        Right cr -> case A.decode . LBS.fromStrict =<< Consumer.crValue cr of
          Nothing -> do
            flow $ logError $ "skipping undecodable message at offset " <> show (Consumer.crOffset cr)
            go lastByPartition handled lastIdleReport
          Just event -> do
            handleWithRetry event
            let lastByPartition' = M.insert (Consumer.crPartition cr) (eventTime event) lastByPartition
            if handled + 1 >= progressEvery
              then progress (minimum (M.elems lastByPartition')) >> go lastByPartition' 0 lastIdleReport
              else go lastByPartition' (handled + 1) lastIdleReport

    handleWithRetry event =
      flow (handler event) `E.catch` \(e :: E.SomeException) -> do
        flow $ logError $ "handler failed, retrying in 1s: " <> show e
        CC.threadDelay 1000000
        handleWithRetry event

    progress watermark = do
      commitErr <- Consumer.commitAllOffsets Consumer.OffsetCommit kc
      case commitErr of
        Just err | isRdKafkaError "RdKafkaRespErrNoOffset" err -> pure () -- nothing consumed since the last commit
        Just err -> flow $ logError $ "offset commit failed: " <> show err
        Nothing -> pure ()
      flow (reportWatermark watermark) `E.catch` \(e :: E.SomeException) ->
        flow $ logError $ "watermark update failed: " <> show e

-- | The librdkafka error codes live in hw-kafka-client's hidden
-- Kafka.Internal.RdKafka module, so match on the code's shown name instead:
-- 'pollMessage' returns @KafkaResponseError RdKafkaRespErrTimedOut@ when nothing
-- arrived within the timeout.
isRdKafkaError :: Text -> KafkaError -> Bool
isRdKafkaError code err = code `T.isInfixOf` T.pack (P.show err)
