{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import System.Environment

runFMDWrapper :: (AppEnv -> AppEnv) -> IO ()
runFMDWrapper configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  let settings =
        setOnExceptionResponse fmdWrapperExceptionResponse $
          setPort (port appEnv) defaultSettings
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        let shortOrgId = appEnv ^. #selfId
        getManager <-
          prepareAuthManager flowRt appEnv "Authorization" shortOrgId
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication manager: "
        authManager <- L.runIO getManager
        try (prepareRedisConnections $ redisCfg appEnv)
          >>= handleLeft exitRedisConnPrepFailure "Exception thrown: " . first (id @SomeException)
        migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
        return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ run $ App.EnvR flowRt' appEnv

fmdWrapperExceptionResponse :: SomeException -> Response
fmdWrapperExceptionResponse = exceptionResponse
