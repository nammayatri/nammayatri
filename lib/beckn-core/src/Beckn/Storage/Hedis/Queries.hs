module Beckn.Storage.Hedis.Queries where

import Beckn.Prelude
import Beckn.Storage.Hedis.Config
import Beckn.Storage.Hedis.Error
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Database.Redis (Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import GHC.Records.Extra

type ExpirationTime = Integer

runHedis ::
  HedisFlow m env => Redis (Either Reply a) -> m a
runHedis action = do
  con <- asks (.hedisEnv.hedisConnection)
  eithRes <- liftIO $ Hedis.runRedis con action
  fromEitherM (HedisReplyError . show) eithRes

runHedisTransaction ::
  HedisFlow m env => RedisTx (Queued a) -> m a
runHedisTransaction action = do
  con <- asks (.hedisEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> throwError $ HedisReplyError err
    TxAborted -> throwError HedisTransactionAborted
    TxSuccess a -> return a

----------------------------------------------------

buildKey :: HedisFlow m env => Text -> m BS.ByteString
buildKey key = do
  keyModifier <- asks (.hedisEnv.keyModifier)
  return . cs $ keyModifier key

runWithPrefix :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m a
runWithPrefix key action = do
  prefKey <- buildKey key
  withLogTag "Redis" $ logDebug $ "working with key : " <> cs prefKey
  runHedis $ action prefKey

runWithPrefix_ :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m ()
runWithPrefix_ key action = void $ runWithPrefix key action

get ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
get key = do
  maybeBS <- runWithPrefix key Hedis.get
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

set ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m ()
set key val = runWithPrefix_ key $ \prefKey ->
  Hedis.set prefKey $ BSL.toStrict $ Ae.encode val

del :: (HedisFlow m env) => Text -> m ()
del key = runWithPrefix_ key $ \prefKey -> Hedis.del [prefKey]

rPushExp :: (HedisFlow m env, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = do
  prefKey <- buildKey key
  unless (null list) $
    void . runHedisTransaction $ do
      void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
      Hedis.expire prefKey ex

lPush :: (HedisFlow m env, ToJSON a) => Text -> [a] -> m ()
lPush key list = runWithPrefix_ key $ \prefKey ->
  Hedis.lpush prefKey $ map (BSL.toStrict . Ae.encode) list

rPush :: (HedisFlow m env, ToJSON a) => Text -> [a] -> m ()
rPush key list = runWithPrefix_ key $ \prefKey ->
  Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list

lTrim :: (HedisFlow m env) => Text -> Integer -> Integer -> m ()
lTrim key start stop = runWithPrefix_ key $ \prefKey ->
  Hedis.ltrim prefKey start stop

lLen :: (HedisFlow m env) => Text -> m Integer
lLen key = runWithPrefix key Hedis.llen

lRange :: (HedisFlow m env, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRange key start stop = do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.lrange prefKey start stop
  mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incrByFloat :: (HedisFlow m env) => Text -> Double -> m Double
incrByFloat key toAdd = runWithPrefix key $ \prefKey ->
  Hedis.incrbyfloat prefKey toAdd
