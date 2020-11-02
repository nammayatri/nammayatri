{-# LANGUAGE TypeApplications #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common (runFlowR)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import Beckn.Utils.Servant.Server (exceptionResponse)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Runtime as E
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )

runFMDWrapper :: (AppEnv -> AppEnv) -> IO ()
runFMDWrapper configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  let loggerCfg = getEulerLoggerConfig $ loggerConfig appEnv
  let settings =
        setOnExceptionResponse fmdWrapperExceptionResponse $
          setPort (port appEnv) defaultSettings
  reqHeadersKey <- V.newKey
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing Redis Connections..."
    try (runFlowR flowRt appEnv $ prepareRedisConnections $ redisCfg appEnv) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
        runSettings settings $ run reqHeadersKey (App.EnvR flowRt appEnv)

fmdWrapperExceptionResponse :: SomeException -> Response
fmdWrapperExceptionResponse = exceptionResponse
