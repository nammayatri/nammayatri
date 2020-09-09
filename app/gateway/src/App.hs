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
import Beckn.Utils.Dhall (ZL (Z), readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server (exceptionResponse)
import qualified Data.Cache as C
import EulerHS.Prelude
import EulerHS.Runtime as E
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

runGateway :: IO ()
runGateway = do
  appCfg@AppCfg {..} <- readDhallConfigDefault Z "beckn-gateway"
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  shutdown <- newEmptyTMVarIO
  activeConnections <- newTVarIO (0 :: Int)
  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing
  let loggerCfg = getEulerLoggerConfig "/tmp/beckn-gateway.log" loggerConfig
      settings =
        setOnExceptionResponse gatewayExceptionResponse $
          setOnOpen (\_ -> atomically $ modifyTVar' activeConnections (+ 1) >> return True) $
            setOnClose (\_ -> atomically $ modifyTVar' activeConnections (subtract 1)) $
              setPort port defaultSettings
  cache <- C.newCache Nothing
  threadId <- forkIO $
    E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
      putStrLn @String "Initializing Redis Connections..."
      try (runFlowR flowRt appCfg $ prepareRedisConnections redisCfg) >>= \case
        Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
        Right _ -> do
          void $ migrateIfNeeded migrationPath dbCfg autoMigrate
          runSettings settings $ run shutdown (App.EnvR flowRt $ mkAppEnv appCfg cache)
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
