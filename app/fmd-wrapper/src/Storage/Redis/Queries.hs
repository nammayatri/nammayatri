module Storage.Redis.Queries where

import App.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

runKV = L.runKVDB "redis"

-- KV
setKeyRedis ::
  forall a.
  A.ToJSON a =>
  Text ->
  a ->
  Flow ()
setKeyRedis key val =
  void $ runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)

setExRedis ::
  forall a.
  A.ToJSON a =>
  Text ->
  a ->
  Int ->
  Flow ()
setExRedis key value ttl =
  void $
    runKV $
      L.setex (DTE.encodeUtf8 key) (toEnum ttl) (BSL.toStrict $ A.encode value)

getKeyRedis ::
  forall a.
  A.FromJSON a =>
  Text ->
  Flow (Maybe a)
getKeyRedis key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) -> A.decode $ BSL.fromStrict v
      _ -> Nothing

getKeyRedisWithError ::
  forall a.
  A.FromJSON a =>
  Text ->
  Flow (Either String a)
getKeyRedisWithError key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) ->
        maybe (Left $ "decode failed for key: " <> show key) Right $
          A.decode (BSL.fromStrict v)
      Right Nothing -> Left $ "No Value found for key : " <> show key
      Left err -> Left $ show err

setHashRedis :: ToJSON a => Text -> Text -> a -> Flow ()
setHashRedis key field value =
  void $
    runKV $
      L.hset
        (DTE.encodeUtf8 key)
        (DTE.encodeUtf8 field)
        (BSL.toStrict $ A.encode value)

expireRedis :: Text -> Int -> Flow ()
expireRedis key ttl = void $ runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)

getHashKeyRedis :: FromJSON a => Text -> Text -> Flow (Maybe a)
getHashKeyRedis key field = do
  resp <- runKV $ L.hget (DTE.encodeUtf8 key) (DTE.encodeUtf8 field)
  return $
    case resp of
      Right (Just v) -> A.decode $ BSL.fromStrict v
      _ -> Nothing

deleteKeyRedis :: Text -> Flow Int
deleteKeyRedis = deleteKeysRedis . return

deleteKeysRedis :: [Text] -> Flow Int
deleteKeysRedis keys = do
  resp <- runKV $ L.del $ map DTE.encodeUtf8 keys
  return $
    case resp of
      Right x -> fromEnum x
      Left err -> -1

incrementKeyRedis :: Text -> Flow (Maybe Integer)
incrementKeyRedis key = do
  val <- runKV . L.incr . DTE.encodeUtf8 $ key
  return $ either (const Nothing) Just val

getKeyRedisText ::
  Text ->
  Flow (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) -> Just $ DTE.decodeUtf8 v
      _ -> Nothing
