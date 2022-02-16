module Environment where

import Beckn.Storage.Hedis
import Beckn.Types.Logging
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Control.Monad.Catch (bracket)
import Relude
import Servant.Client

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    hedisCfg :: HedisCfg,
    statusWaitTimeSec :: Int,
    callbackWaitTimeMilliSec :: Int,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hedisEnv <- connectHedis hedisCfg ("mock_public_transport_bpp" <>)
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  disconnectHedis hedisEnv
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv
