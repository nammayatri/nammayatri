{-# LANGUAGE OverloadedLabels #-}
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
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
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
  let loggerCfg = getEulerLoggerConfig $ appEnv ^. #loggerConfig
  let settings =
        setOnExceptionResponse fmdWrapperExceptionResponse $
          setPort (port appEnv) defaultSettings
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing Redis Connections..."
    let shortOrgId = appEnv ^. #selfId
    case prepareAuthManager flowRt appEnv "Authorization" shortOrgId of
      Left err -> putStrLn @String ("Could not prepare authentication manager: " <> show err)
      Right getManager -> do
        authManager <- getManager
        let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
        try (runFlowR flowRt' appEnv $ prepareRedisConnections $ redisCfg appEnv) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ -> do
            _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
            runSettings settings $ run $ App.EnvR flowRt' appEnv

fmdWrapperExceptionResponse :: SomeException -> Response
fmdWrapperExceptionResponse = exceptionResponse
