module App.Types where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions (..))
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import qualified Data.Text as T
import System.Environment (lookupEnv)

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    selfId :: Text,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity'
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- fmap T.pack <$> lookupEnv "POD_NAME"
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getRegistry = (.config.authEntity.credRegistry)
  getSigningKeys = (.config.authEntity.signingKeys)
  getSignatureExpiry = (.config.authEntity.signatureExpiry)
