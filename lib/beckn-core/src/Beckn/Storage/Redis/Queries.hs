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
import Beckn.Utils.Error.Throwing (throwRedisError)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types as T

runKV :: HasCallStack => L.KVDB a -> FlowR r (T.KVDBAnswer a)
runKV = L.runKVDB "redis"

-- KV
setKeyRedis ::
  ( HasCallStack,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  FlowR r ()
setKeyRedis key val = do
  resp <- runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)
  case resp of
    -- TODO: check for "OK" in resp
    Right _ -> return ()
    Left err -> throwRedisError $ show err

setExRedis ::
  ( HasCallStack,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  Int ->
  FlowR r ()
setExRedis key value ttl = do
  resp <- runKV $ L.setex (DTE.encodeUtf8 key) (toEnum ttl) (BSL.toStrict $ A.encode value)
  case resp of
    -- TODO: check for "OK" in resp
    Right _ -> return ()
    Left err -> throwRedisError $ show err

getKeyRedis ::
  ( HasCallStack,
    A.FromJSON a
  ) =>
  Text ->
  FlowR r (Maybe a)
getKeyRedis key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  case resp of
    Right (Just v) -> return $ A.decode $ BSL.fromStrict v
    Right Nothing -> return Nothing
    Left err -> throwRedisError $ show err

getKeyRedisWithError ::
  ( HasCallStack,
    A.FromJSON a
  ) =>
  Text ->
  FlowR r (Either String a)
getKeyRedisWithError key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  case resp of
    Right (Just v) ->
      return $
        maybe (Left $ "decode failed for key: " <> show key) Right $
          A.decode (BSL.fromStrict v)
    Right Nothing -> return $ Left $ "No Value found for key : " <> show key
    Left err -> throwRedisError $ show err

setHashRedis ::
  ( HasCallStack,
    ToJSON a
  ) =>
  Text ->
  Text ->
  a ->
  FlowR r ()
setHashRedis key field value = do
  resp <-
    runKV $
      L.hset
        (DTE.encodeUtf8 key)
        (DTE.encodeUtf8 field)
        (BSL.toStrict $ A.encode value)
  case resp of
    -- TODO: check for "OK" in resp
    Right _ -> return ()
    Left err -> throwRedisError $ show err

expireRedis ::
  HasCallStack =>
  Text ->
  Int ->
  FlowR r ()
expireRedis key ttl = do
  resp <- runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)
  case resp of
    Right _ -> return ()
    Left err -> throwRedisError $ show err

getHashKeyRedis ::
  ( HasCallStack,
    FromJSON a
  ) =>
  Text ->
  Text ->
  FlowR r (Maybe a)
getHashKeyRedis key field = do
  resp <- runKV $ L.hget (DTE.encodeUtf8 key) (DTE.encodeUtf8 field)
  case resp of
    Right (Just v) -> return $ A.decode $ BSL.fromStrict v
    Right Nothing -> return Nothing
    Left err -> throwRedisError $ show err

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
  case resp of
    Right x -> return $ fromEnum x
    Left err -> throwRedisError $ show err

incrementKeyRedis ::
  HasCallStack =>
  Text ->
  FlowR r Integer
incrementKeyRedis key = do
  resp <- runKV . L.incr . DTE.encodeUtf8 $ key
  case resp of
    Right x -> return x
    Left err -> throwRedisError $ show err

getKeyRedisText ::
  HasCallStack =>
  Text ->
  FlowR r (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  case resp of
    Right (Just v) -> return $ Just $ DTE.decodeUtf8 v
    Right Nothing -> return Nothing
    Left err -> throwRedisError $ show err

tryLockRedis ::
  HasCallStack =>
  Text ->
  Int ->
  FlowR r Bool
tryLockRedis key expire = do
  resp <- runKV (L.rawRequest ["SET", buildLockResourceName key, "1", "NX", "EX", maxLockTime])
  case resp of
    Right (Just ("OK" :: ByteString)) -> return True
    Right _ -> return False
    Left err -> throwRedisError $ show err
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
