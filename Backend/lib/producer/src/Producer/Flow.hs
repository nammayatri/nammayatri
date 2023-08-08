{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Producer.Flow where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.CacheFlow
import Kernel.Types.Flow ()
import Kernel.Utils.Common
import Kernel.Utils.Time ()

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches batchSize xs =
  let (batch, rest) = splitAt batchSize xs
   in batch : splitIntoBatches batchSize rest

getTime :: (CacheFlow m r) => Double -> Text -> m Double
getTime begTime producerTimestampKey = do
  Hedis.safeGet producerTimestampKey <&> \case
    Just lastTime -> lastTime
    Nothing -> begTime

runProducer :: Flow ()
runProducer = do
  begTime <- getCurrentTimestamp
  producerTimestampKey <- asks (.producerTimestampKey)
  startTime <- getTime begTime producerTimestampKey
  endTime <- getCurrentTimestamp
  Hedis.set producerTimestampKey endTime
  setName <- asks (.schedulerSetName)
  currentJobs <- Hedis.withNonCriticalCrossAppRedis $ Hedis.zRangeByScore setName startTime endTime
  _ <- Hedis.withNonCriticalCrossAppRedis $ Hedis.zRemRangeByScore setName startTime endTime
  logDebug $ "Jobs taken out of sortedset " <> show currentJobs
  batchSize <- asks (.batchSize)
  let jobChunks = splitIntoBatches batchSize currentJobs
  logDebug $ "Job chunks producer" <> show jobChunks
  forM_ jobChunks $ \chunk -> do
    streamName <- asks (.streamName)
    entryId <- asks (.entryId)
    eqId <- generateGUID
    let eqIdByteString = TE.encodeUtf8 eqId
    let chunk_ = B.concat chunk
    let fieldValue = [(eqIdByteString, chunk_)]
    result <- Hedis.withNonCriticalCrossAppRedis $ Hedis.xAdd streamName entryId fieldValue
    logDebug $ "Jobs inserted into stream" <> show result
  endTime <- getCurrentTimestamp
  let diff = endTime - begTime
  waitTimeMilliSec <- asks (.waitTimeMilliSec)
  threadDelayMilliSec . Milliseconds $ max 0 (fromEnum (waitTimeMilliSec - diff) :: Int)
  fork "" $ addGenericLatency "producer" $ fromIntegral $ fromEnum diff
  runProducer
