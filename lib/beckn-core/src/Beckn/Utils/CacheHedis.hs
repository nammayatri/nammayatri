{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.CacheHedis where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Logging
import Beckn.Utils.Error.Throwing
import qualified Control.Monad.Catch as C
import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Database.Redis hiding (runRedis)
import qualified Database.Redis as Hedis
import GHC.Records.Extra
import Type.Reflection

newtype HedisEnv = HedisEnv
  { redisConnection :: Connection
  }

withHedisEnv :: (HedisEnv -> IO a) -> IO a
withHedisEnv = C.bracket (HedisEnv <$> connectRedis) (disconnect . redisConnection)

connectRedis :: IO Connection
connectRedis = checkedConnect defaultConnectInfo

----------------------------------------------------

newtype HedisReplyError = HedisReplyError Reply
  deriving (Show, Typeable, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HedisReplyError

instance IsBaseError HedisReplyError where
  toMessage = \case
    HedisReplyError err -> Just $ show err

instance IsHTTPError HedisReplyError where
  toErrorCode = \case
    HedisReplyError _ -> "REDIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError HedisReplyError

----------------------------------------------------

newtype HedisDecodeError = HedisDecodeError Text
  deriving (Show, Typeable, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HedisDecodeError

instance IsBaseError HedisDecodeError where
  toMessage = \case
    HedisDecodeError err -> Just $ show err

instance IsHTTPError HedisDecodeError where
  toErrorCode = \case
    HedisDecodeError _ -> "REDIS_DECODE_ERROR"
  toHttpCode _ = E500

instance IsAPIError HedisDecodeError

----------------------------------------------------

type MonadHedis env m =
  (MonadReader env m, HasField "hedisEnv" env HedisEnv, MonadIO m, C.MonadThrow m, Log m)

runHedis ::
  MonadHedis env m => Redis (Either Reply a) -> m a
runHedis action = do
  con <- asks (.hedisEnv.redisConnection)
  eithRes <- liftIO $ Hedis.runRedis con action
  fromEitherM HedisReplyError eithRes

----------------------------------------------------

mkKey :: Text -> Text -> BS.ByteString
mkKey prefix key = cs $ prefix <> ":" <> key

getKey ::
  (FromJSON a, MonadHedis env m) => Text -> Text -> m (Maybe a)
getKey prefix key = do
  let prefKey = mkKey prefix key
  maybeBS <- runHedis $ get prefKey
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

setKey ::
  (ToJSON a, MonadHedis env m) => Text -> Text -> a -> m ()
setKey prefix key val = do
  let prefKey = cs $ mkKey prefix key
  _ <- runHedis $ set prefKey $ BSL.toStrict $ Ae.encode val
  pure ()

delKey :: (MonadHedis env m) => Text -> Text -> m ()
delKey prefix key = do
  let prefKey = mkKey prefix key
  void $ runHedis $ del [prefKey]
