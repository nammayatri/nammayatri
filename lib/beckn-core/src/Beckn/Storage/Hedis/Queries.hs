module Beckn.Storage.Hedis.Queries
  ( get,
    set,
    del,
    getList,
    rPush,
    rPushExp,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis.Config
import Beckn.Storage.Hedis.Error
import Beckn.Utils.Error.Throwing
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
  prefix <- asks (.hedisEnv.hedisPrefix)
  return . cs $ prefix <> ":" <> key

get ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
get key = do
  prefKey <- buildKey key
  maybeBS <- runHedis $ Hedis.get prefKey
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

set ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m ()
set key val = do
  prefKey <- buildKey key
  void . runHedis $ Hedis.set prefKey $ BSL.toStrict $ Ae.encode val

del :: (HedisFlow m env) => Text -> m ()
del key = do
  prefKey <- buildKey key
  void $ runHedis $ Hedis.del [prefKey]

getList :: forall a (m :: Type -> Type) env. (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = do
  prefKey <- buildKey key
  res <- runHedis $ do
    len <- Hedis.llen prefKey
    join <$> mapM (Hedis.lrange prefKey 0) len
  mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ BSL.fromStrict a) res

rPush :: (HedisFlow m env, ToJSON a) => Text -> [a] -> m ()
rPush key list = do
  prefKey <- buildKey key
  void . runHedis . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list

rPushExp :: (HedisFlow m env, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = do
  prefKey <- buildKey key
  unless (null list) $
    void . runHedisTransaction $ do
      void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
      Hedis.expire prefKey ex
