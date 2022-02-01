{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.CacheHedis where

--import Control.Monad.Trans.Except

import Beckn.Prelude
--import Beckn.External.MyValueFirst.Types (SubmitSmsRes, submitSmsResToText)
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
--import Beckn.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
--import Beckn.Utils.Servant.BaseUrl
--import Network.HTTP.Types (Header, Status (statusCode))
--import Network.HTTP.Types.Header (HeaderName)
--import Servant.Client (BaseUrl, ClientError, ResponseF (responseStatusCode))

import Beckn.Types.Logging
import Beckn.Utils.Error.Hierarchy
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

----------------------------------------------------

newtype HedisError = HedisError Reply
  deriving (Show, Typeable, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HedisError

instance IsBaseError HedisError where
  toMessage = \case
    HedisError err -> Just $ show err

instance IsHTTPError HedisError where
  toErrorCode = \case
    HedisError _ -> "REDIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError HedisError

----------------------------------------------------

type MonadHedis env m =
  (MonadReader env m, HasField "hedisEnv" env HedisEnv, MonadIO m, C.MonadThrow m, Log m)

runHedis ::
  MonadHedis env m => Redis (Either Reply a) -> m a
runHedis action = do
  con <- asks (.hedisEnv.redisConnection)
  eithRes <- liftIO $ Hedis.runRedis con action
  fromEitherM HedisError eithRes

----------------------------------------------------

mkKey :: Text -> Text -> BS.ByteString
mkKey prefix key = cs $ prefix <> ":" <> key

getKey ::
  (FromJSON a, MonadHedis env m) => Text -> Text -> m (Maybe a)
getKey prefix key = do
  let prefKey = mkKey prefix key
  maybeBS <- runHedis $ get prefKey
  pure $ Ae.decode . BSL.fromStrict =<< maybeBS

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
