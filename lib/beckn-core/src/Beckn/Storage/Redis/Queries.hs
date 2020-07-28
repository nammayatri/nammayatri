module Beckn.Storage.Redis.Queries where

import Beckn.Types.App
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types as T

runKV :: L.MonadFlow mFlow => L.KVDB a -> mFlow (T.KVDBAnswer a)
runKV = L.runKVDB "redis"

-- KV
setKeyRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  mFlow ()
setKeyRedis key val =
  void $ runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)

setExRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  Int ->
  mFlow ()
setExRedis key value ttl =
  void $
    runKV $
      L.setex (DTE.encodeUtf8 key) (toEnum ttl) (BSL.toStrict $ A.encode value)

getKeyRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    A.FromJSON a
  ) =>
  Text ->
  mFlow (Maybe a)
getKeyRedis key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) -> A.decode $ BSL.fromStrict v
      _ -> Nothing

getKeyRedisWithError ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    A.FromJSON a
  ) =>
  Text ->
  mFlow (Either String a)
getKeyRedisWithError key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) ->
        maybe (Left $ "decode failed for key: " <> show key) Right $
          A.decode (BSL.fromStrict v)
      Right Nothing -> Left $ "No Value found for key : " <> show key
      Left err -> Left $ show err

setHashRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    ToJSON a
  ) =>
  Text ->
  Text ->
  a ->
  mFlow ()
setHashRedis key field value =
  void $
    runKV $
      L.hset
        (DTE.encodeUtf8 key)
        (DTE.encodeUtf8 field)
        (BSL.toStrict $ A.encode value)

expireRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow
  ) =>
  Text ->
  Int ->
  mFlow ()
expireRedis key ttl = void $ runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)

getHashKeyRedis ::
  ( L.MonadFlow mFlow,
    HasRedisEnv mFlow,
    FromJSON a
  ) =>
  Text ->
  Text ->
  mFlow (Maybe a)
getHashKeyRedis key field = do
  resp <- runKV $ L.hget (DTE.encodeUtf8 key) (DTE.encodeUtf8 field)
  return $
    case resp of
      Right (Just v) -> A.decode $ BSL.fromStrict v
      _ -> Nothing

deleteKeyRedis ::
  (L.MonadFlow mFlow, HasRedisEnv mFlow) =>
  Text ->
  mFlow Int
deleteKeyRedis = deleteKeysRedis . return

deleteKeysRedis ::
  (L.MonadFlow mFlow, HasRedisEnv mFlow) =>
  [Text] ->
  mFlow Int
deleteKeysRedis keys = do
  resp <- runKV $ L.del $ map DTE.encodeUtf8 keys
  return $
    case resp of
      Right x -> fromEnum x
      Left err -> -1

incrementKeyRedis ::
  (L.MonadFlow mFlow, HasRedisEnv mFlow) =>
  Text ->
  mFlow (Maybe Integer)
incrementKeyRedis key = do
  val <- runKV . L.incr . DTE.encodeUtf8 $ key
  return $ either (const Nothing) Just val

getKeyRedisText ::
  (L.MonadFlow mFlow, HasRedisEnv mFlow) =>
  Text ->
  mFlow (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) -> Just $ DTE.decodeUtf8 v
      _ -> Nothing
