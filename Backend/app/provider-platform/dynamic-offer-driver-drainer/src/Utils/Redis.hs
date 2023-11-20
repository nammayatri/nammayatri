module Utils.Redis where

import Config.Env as Env
import Data.Aeson
import qualified Data.Bifunctor as Bifunctor
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Database.Redis as R
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Types.DBSync

getRedisName :: Flow Text
getRedisName = do
  Env {..} <- ask
  pure _streamRedisInfo

addValueToRedisStream ::
  Text ->
  L.KVDBStreamEntryIDInput ->
  [(Text, Text)] ->
  Flow (Either ET.KVDBReply L.KVDBStreamEntryID)
addValueToRedisStream streamKey streamEntryInfo value = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.xadd (DTE.encodeUtf8 streamKey) streamEntryInfo (Bifunctor.bimap DTE.encodeUtf8 DTE.encodeUtf8 <$> value)

addValueToErrorQueue :: ConvertUtf8 a L.KVDBStream => a -> [L.KVDBStreamItem] -> ReaderT Env L.Flow (Either ET.KVDBReply ())
addValueToErrorQueue streamKey values = do
  redisToUse <- getRedisName
  kvRes <- L.runKVDB redisToUse $ L.xadd (encodeUtf8 streamKey) L.AutoID values
  case kvRes of
    Left x -> pure $ Left x
    Right _ -> pure $ Right ()

getRedisStreamLength ::
  Text ->
  Flow (Either ET.KVDBReply Integer)
getRedisStreamLength streamKey = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.xlen (DTE.encodeUtf8 streamKey)

readValueFromRedisStream ::
  Text ->
  L.KVDBStreamEntryIDInput ->
  Integer ->
  Flow (Either ET.KVDBReply (Maybe [R.XReadResponse]))
readValueFromRedisStream streamKey streamInput count = do
  redisToUse <- getRedisName
  streamBlockTime <- L.runIO Env.getStreamBlockTime
  L.runKVDB redisToUse $ L.xreadOpts [(DTE.encodeUtf8 streamKey, streamInput)] $ R.XReadOpts streamBlockTime (Just count) False

deleteStreamValue ::
  Text ->
  [L.KVDBStreamEntryID] ->
  Flow (Either ET.KVDBReply Integer)
deleteStreamValue streamKey entryIds = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.xdel (DTE.encodeUtf8 streamKey) entryIds

incrementCounter ::
  Text ->
  Flow (Either ET.KVDBReply Integer)
incrementCounter redisKey = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.incr (DTE.encodeUtf8 redisKey)

setValueWithOptions ::
  Text ->
  Text ->
  L.KVDBSetTTLOption ->
  L.KVDBSetConditionOption ->
  Flow (Either ET.KVDBReply Bool)
setValueWithOptions key value ttl setOpts = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.setOpts (DTE.encodeUtf8 key) (DTE.encodeUtf8 value) ttl setOpts

deleteKey ::
  [Text] ->
  Flow (Either ET.KVDBReply Integer)
deleteKey key = do
  redisToUse <- getRedisName
  L.runKVDB redisToUse $ L.del (DTE.encodeUtf8 <$> key)

readHashKey :: Read a => Text -> Text -> Flow (Either Text a)
readHashKey key value = do
  val <- getHashKey key value
  pure (maybeToRight ("Unable to get from redis " <> key) (readMaybe =<< val))

getHashKey :: Text -> Text -> Flow (Maybe String)
getHashKey key value = do
  redisToUse <- getRedisName
  (T.unpack <$>) <$> (fmap DTE.decodeUtf8 <$> L.rHgetB redisToUse (DTE.encodeUtf8 key) (DTE.encodeUtf8 value))

getValueFromRedis :: Text -> Flow (Maybe Bool)
getValueFromRedis key = do
  redisToUse <- getRedisName
  L.rGet redisToUse key

setValueInRedis :: ToJSON v => Text -> v -> Flow (Either ET.KVDBReply ET.KVDBStatus)
setValueInRedis key value = do
  redisToUse <- getRedisName
  L.rSet redisToUse key value
