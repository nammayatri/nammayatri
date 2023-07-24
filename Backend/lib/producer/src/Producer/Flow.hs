{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Producer.Flow where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (pack)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common

getCurrentTimestamp :: IO Double
getCurrentTimestamp = realToFrac <$> getPOSIXTime

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches batchSize xs =
  let (batch, rest) = splitAt batchSize xs
   in batch : splitIntoBatches batchSize rest

-- arrayToByteString :: [BS.ByteString] -> BS.ByteString
-- arrayToByteString chunk = BSL.toStrict $ Ae.encode $ BSL.fromChunks chunk

-- Concatenate the list of strict ByteStrings into a single strict ByteString
concatenateChunks :: [BS.ByteString] -> BS.ByteString
concatenateChunks = BS.concat

-- Encode the ByteString to JSON using Aeson
encodeToJSON :: BS.ByteString -> BSL.ByteString
encodeToJSON = Ae.encode . TE.decodeUtf8

arrayToByteString :: [BS.ByteString] -> BS.ByteString
arrayToByteString chunks = BSL.toStrict (encodeToJSON (concatenateChunks chunks))

runProducer :: Flow ()
runProducer = do
  currentTime <- liftIO getCurrentTimestamp
  let oneSecondAgo = currentTime - 1
  print $ pack "Producer is running ..."
  setName <- asks (.setName)
  currentJobs <- Hedis.zrangebyscore setName oneSecondAgo currentTime
  logDebug $ "Jobs taken out of sortedset" <> show currentJobs

  batchSize <- asks (.batchSize)
  let jobChunks = splitIntoBatches batchSize currentJobs
  logDebug $ "Job chunks producer" <> show jobChunks

  forM_ jobChunks $ \chunk -> do
    streamName <- asks (.streamName)
    entryId <- asks (.entryId)
    eqId <- generateGUID
    let eqIdByteString = TE.encodeUtf8 eqId
    let chunk_ = arrayToByteString chunk
    let fieldValue = [(eqIdByteString, chunk_)]
    result <- Hedis.xadd streamName entryId fieldValue
    logDebug $ "Jobs inserted out of stream" <> show result

-- pure ()
