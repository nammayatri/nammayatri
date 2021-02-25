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
    lockedSinceRedis,
  )
where

import Beckn.Utils.Common
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types as T

runKV :: (HasCallStack, L.MonadFlow mFlow) => L.KVDB a -> mFlow (T.KVDBAnswer a)
runKV = L.runKVDB "redis"

-- KV
setKeyRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  mFlow ()
setKeyRedis key val =
  void $ runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)

setExRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow,
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
  ( HasCallStack,
    L.MonadFlow mFlow,
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
  ( HasCallStack,
    L.MonadFlow mFlow,
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
  ( HasCallStack,
    L.MonadFlow mFlow,
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
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  Int ->
  mFlow ()
expireRedis key ttl = void $ runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)

getHashKeyRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow,
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
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  mFlow Int
deleteKeyRedis = deleteKeysRedis . return

deleteKeysRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  [Text] ->
  mFlow Int
deleteKeysRedis rKeys = do
  resp <- runKV $ L.del $ map DTE.encodeUtf8 rKeys
  return $
    case resp of
      Right x -> fromEnum x
      Left _ -> -1

incrementKeyRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  mFlow (Maybe Integer)
incrementKeyRedis key = do
  val <- runKV . L.incr . DTE.encodeUtf8 $ key
  return $ either (const Nothing) Just val

getKeyRedisText ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  mFlow (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Right (Just v) -> Just $ DTE.decodeUtf8 v
      _ -> Nothing

tryLockRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  Int ->
  mFlow Bool
tryLockRedis key expire = do
  now <- getCurrTime
  resp <- runKV (L.rawRequest ["SET", buildLockResourceName key, BSL.toStrict $ A.encode now, "NX", "EX", maxLockTime])
  return $
    case resp of
      Right (Just ("OK" :: ByteString)) -> True
      _ -> False
  where
    maxLockTime = show expire

unlockRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  mFlow ()
unlockRedis key = do
  _ <- deleteKeyRedis $ buildLockResourceName key
  return ()

lockedSinceRedis ::
  ( HasCallStack,
    L.MonadFlow mFlow
  ) =>
  Text ->
  mFlow (Maybe UTCTime)
lockedSinceRedis key =
  getKeyRedis (buildLockResourceName key)

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "beckn:locker:" <> DT.unpack key