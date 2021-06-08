module Beckn.Storage.Redis.Queries
  ( setKeyRedis,
    setExRedis,
    getKeyRedis,
    getKeyRedisWithError,
    setHashRedis,
    expireRedis,
    getHashKeyRedis,
    deleteKeyRedis,
    deleteKeysRedis,
    incrementKeyRedis,
    getKeyRedisText,
    tryLockRedis,
    unlockRedis,
  )
where

import Beckn.Types.Common
import Beckn.Types.Error.API (RedisError (..))
import Beckn.Utils.Error.Throwing (fromEitherM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

runKV :: HasCallStack => L.KVDB a -> FlowR r a
runKV = L.runKVDB "redis" >=> fromEitherM RedisError

-- KV
setKeyRedis ::
  ( HasCallStack,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  FlowR r ()
setKeyRedis key val =
  -- TODO: check for "OK" in resp
  void $ runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)

setExRedis ::
  ( HasCallStack,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  Int ->
  FlowR r ()
setExRedis key value ttl =
  -- TODO: check for "OK" in resp
  void $ runKV $ L.setex (DTE.encodeUtf8 key) (toEnum ttl) (BSL.toStrict $ A.encode value)

getKeyRedis ::
  ( HasCallStack,
    A.FromJSON a
  ) =>
  Text ->
  FlowR r (Maybe a)
getKeyRedis key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $ A.decode . BSL.fromStrict =<< resp

getKeyRedisWithError ::
  ( HasCallStack,
    A.FromJSON a
  ) =>
  Text ->
  FlowR r (Either String a)
getKeyRedisWithError key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Just v ->
        maybe (Left $ "decode failed for key: " <> show key) Right $
          A.decode (BSL.fromStrict v)
      Nothing -> Left $ "No Value found for key : " <> show key

setHashRedis ::
  ( HasCallStack,
    ToJSON a
  ) =>
  Text ->
  Text ->
  a ->
  FlowR r ()
setHashRedis key field value =
  -- TODO: check for "OK" in resp
  void $
    runKV $
      L.hset
        (DTE.encodeUtf8 key)
        (DTE.encodeUtf8 field)
        (BSL.toStrict $ A.encode value)

expireRedis ::
  HasCallStack =>
  Text ->
  Int ->
  FlowR r ()
expireRedis key ttl =
  void $ runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)

getHashKeyRedis ::
  ( HasCallStack,
    FromJSON a
  ) =>
  Text ->
  Text ->
  FlowR r (Maybe a)
getHashKeyRedis key field = do
  resp <- runKV $ L.hget (DTE.encodeUtf8 key) (DTE.encodeUtf8 field)
  return $ A.decode . BSL.fromStrict =<< resp

deleteKeyRedis ::
  HasCallStack =>
  Text ->
  FlowR r Int
deleteKeyRedis = deleteKeysRedis . return

deleteKeysRedis ::
  HasCallStack =>
  [Text] ->
  FlowR r Int
deleteKeysRedis rKeys = do
  resp <- runKV $ L.del $ map DTE.encodeUtf8 rKeys
  return $ fromEnum resp

incrementKeyRedis ::
  HasCallStack =>
  Text ->
  FlowR r Integer
incrementKeyRedis =
  runKV . L.incr . DTE.encodeUtf8

getKeyRedisText ::
  HasCallStack =>
  Text ->
  FlowR r (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $ DTE.decodeUtf8 <$> resp

tryLockRedis ::
  HasCallStack =>
  Text ->
  Int ->
  FlowR r Bool
tryLockRedis key expire = do
  resp <- runKV (L.rawRequest ["SET", buildLockResourceName key, "1", "NX", "EX", maxLockTime])
  case resp of
    Just ("OK" :: ByteString) -> return True
    _ -> return False
  where
    maxLockTime = show expire

unlockRedis ::
  HasCallStack =>
  Text ->
  FlowR r ()
unlockRedis key = do
  _ <- deleteKeyRedis $ buildLockResourceName key
  return ()

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "beckn:locker:" <> DT.unpack key
