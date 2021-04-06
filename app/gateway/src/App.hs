{-# LANGUAGE TypeApplications #-}

module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import qualified Data.Map.Strict as Map
import EulerHS.Prelude
import EulerHS.Runtime as E
import qualified EulerHS.Runtime as R
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnClose,
    setOnExceptionResponse,
    setOnOpen,
    setPort,
  )
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

runGateway :: (AppCfg -> AppCfg) -> IO ()
runGateway configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-gateway"
  let port = appCfg ^. #port
  let metricsPort = appCfg ^. #metricsPort
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  shutdown <- newEmptyTMVarIO
  activeConnections <- newTVarIO (0 :: Int)
  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing
  let loggerRt = getEulerLoggerRuntime $ appCfg ^. #loggerConfig
      settings =
        setOnExceptionResponse gatewayExceptionResponse $
          setOnOpen (\_ -> atomically $ modifyTVar' activeConnections (+ 1) >> return True) $
            setOnClose (\_ -> atomically $ modifyTVar' activeConnections (subtract 1)) $
              setPort port defaultSettings
  let redisCfg = appCfg ^. #redisCfg
  let migrationPath = appCfg ^. #migrationPath
  let dbCfg = appCfg ^. #dbCfg
  let autoMigrate = appCfg ^. #autoMigrate
  cache <- C.newCache Nothing
  threadId <- forkIO $
    E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
      let appEnv = mkAppEnv appCfg cache
      let shortOrgId = appEnv ^. #gwId
      case prepareAuthManager flowRt appEnv "Proxy-Authorization" shortOrgId of
        Left err -> do
          putStrLn @String ("Could not prepare authentication manager: " <> show err)
          handleShutdown shutdown
        Right getManager -> do
          authManager <- getManager
          let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
          putStrLn @String "Initializing Redis Connections..."
          try (runFlowR flowRt' appCfg $ prepareRedisConnections redisCfg) >>= \case
            Left (e :: SomeException) -> do
              putStrLn @String ("Exception thrown: " <> show e)
              handleShutdown shutdown
            Right _ -> do
              void $ migrateIfNeeded migrationPath dbCfg autoMigrate
              runSettings settings $ run shutdown (App.EnvR flowRt' appEnv)
  -- Wait for shutdown
  atomically $ readTMVar shutdown
  -- Wait to drain all connections
  putStrLn @String "Draining connections"
  waitForDrain activeConnections 120000000
  -- Kill the thread
  killThread threadId
  exitSuccess
  where
    handleShutdown :: TMVar () -> IO ()
    handleShutdown shutdown = liftIO . atomically $ putTMVar shutdown ()

    waitForDrain :: TVar Int -> Int -> IO ()
    waitForDrain activeConnections ms = do
      conns <- readTVarIO activeConnections
      unless (ms == 0 || conns == 0) $ do
        -- Wait 100ms and recurse
        let sleep = 100000
        threadDelay sleep
        waitForDrain activeConnections $ ms - sleep

gatewayExceptionResponse :: SomeException -> Response
gatewayExceptionResponse = exceptionResponse
