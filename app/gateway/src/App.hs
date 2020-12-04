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
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManager, signatureAuthManagerKey)
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
  appCfg@AppCfg {..} <- configModifier <$> readDhallConfigDefault "beckn-gateway"
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  shutdown <- newEmptyTMVarIO
  activeConnections <- newTVarIO (0 :: Int)
  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing
  let loggerCfg = getEulerLoggerConfig loggerConfig
      settings =
        setOnExceptionResponse gatewayExceptionResponse $
          setOnOpen (\_ -> atomically $ modifyTVar' activeConnections (+ 1) >> return True) $
            setOnClose (\_ -> atomically $ modifyTVar' activeConnections (subtract 1)) $
              setPort port defaultSettings
  cache <- C.newCache Nothing
  threadId <- forkIO $
    E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
      let appEnv = mkAppEnv appCfg cache
      authManager <- prepareAuthManager flowRt appCfg appEnv
      let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
      putStrLn @String "Initializing Redis Connections..."
      try (runFlowR flowRt' appCfg $ prepareRedisConnections redisCfg) >>= \case
        Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
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

    prepareAuthManager flowRt cfg appEnv = do
      let selfId = cfg ^. #selfId
      creds <-
        runFlowR flowRt appEnv $
          Registry.lookupOrg selfId
            >>= maybe (error $ "No creds found for id: " <> selfId) pure
      let keyId = creds ^. #_keyId
      privateKey <-
        maybe (error $ "No private key found for credential: " <> show keyId) pure (Registry.decodeKey <$> creds ^. #_signPrivKey)
          >>= maybe (error $ "No private key to decode: " <> fromMaybe "No Key" (creds ^. #_signPrivKey)) pure
      signatureAuthManager flowRt appEnv "Proxy-Authorization" privateKey selfId keyId (appEnv ^. #signatureExpiry)

gatewayExceptionResponse :: SomeException -> Response
gatewayExceptionResponse = exceptionResponse
