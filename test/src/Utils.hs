{-# LANGUAGE TypeApplications #-}

module Utils where

import qualified App.BackgroundTaskManager.Types as BecknTransport
import qualified "beckn-transport" App.Types as BecknTransport
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfig)
import Beckn.Utils.FlowLogging (getEulerLoggerRuntime)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import HSpec
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client hiding (client)

defaultTestLoggerConfig :: LoggerConfig
defaultTestLoggerConfig =
  LoggerConfig
    { level = DEBUG,
      logToFile = True,
      logFilePath = "/tmp/beckn-integ-test.log",
      logToConsole = False,
      logRawSql = True
    }

runClient :: (HasCallStack, MonadIO m) => ClientEnv -> ClientM a -> m (Either ClientError a)
runClient clientEnv x = liftIO $ runClientM x clientEnv

runClient' :: (HasCallStack, MonadIO m, Show a) => ClientEnv -> ClientM a -> m a
runClient' clientEnv x = do
  res <- runClient clientEnv x
  res `shouldSatisfy` isRight
  let Right r = res
  return r

-- | Invoke an action until getting 'Just'.
--
-- The first argument describes attempted delays prior to running an action,
-- in mcs.
pollWith :: (HasCallStack, MonadIO m, MonadCatch m) => [Int] -> m (Maybe a) -> m a
pollWith allDelays action = withFrozenCallStack $ go allDelays
  where
    go [] =
      expectationFailure $
        "poll: failed to get an expected entry after "
          <> show (fromIntegral (sum allDelays) / 1e6 :: Float)
          <> " seconds"
    go (delay : remDelays) = do
      let printLastError err = do
            when (null remDelays) $ print ("Last error: " <> show err :: Text)
            return Nothing
      liftIO $ threadDelay delay
      try @_ @SomeException action >>= either printLastError return >>= maybe (go remDelays) pure

expBackoff :: Int -> Int -> [Int]
expBackoff startDelay maxDelay =
  0 : takeWhile (< maxDelay) (iterate (* 2) startDelay)

-- | 'pollWith' with default timing.
--
-- Optimized for requesting a server for a result of async action.
poll :: (HasCallStack, MonadIO m, MonadCatch m) => m (Maybe a) -> m a
poll = pollWith (expBackoff 0.1e6 10e6)

getLoggerCfg :: String -> T.LoggerConfig
getLoggerCfg t =
  T.defaultLoggerConfig
    { T._logToFile = True,
      T._logFilePath = "/tmp/log-" <> t,
      T._isAsync = False
    }

runFlow :: MonadIO m => Text -> env -> FlowR env a -> m a
runFlow tag appEnv flow = do
  let loggerRt = getEulerLoggerRuntime (Just "Test_Transport_flow") defaultTestLoggerConfig
  liftIO $
    R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
      runFlowR flowRt appEnv $ withLogTag tag flow

expectSingletonNE :: (HasCallStack, MonadIO m) => NonEmpty a -> m a
expectSingletonNE = \case
  a :| [] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

expectSingletonList :: (HasCallStack, MonadIO m) => [a] -> m a
expectSingletonList = \case
  [a] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

data ClientEnvs = ClientEnvs
  { bap :: ClientEnv,
    bpp :: ClientEnv
  }

type ClientsM = ReaderT ClientEnvs IO

withBecknClients :: ClientEnvs -> ClientsM a -> IO a
withBecknClients = flip runReaderT

callBAP, callBPP :: (HasCallStack, Show a) => ClientM a -> ClientsM a
callBAP client = asks (.bap) >>= (`runClient'` client)
callBPP client = asks (.bpp) >>= (`runClient'` client)

mkMobilityClients :: BaseUrl -> BaseUrl -> IO ClientEnvs
mkMobilityClients bapUrl bppUrl = do
  appManager <- Client.newManager tlsManagerSettings
  pure $
    ClientEnvs
      { bap = mkClientEnv appManager bapUrl,
        bpp = mkClientEnv appManager bppUrl
      }

runTransporterFlow :: Text -> FlowR BecknTransport.AppEnv a -> IO a
runTransporterFlow tag flow = do
  (appEnv :: BecknTransport.AppEnv) <- BecknTransport.buildAppEnv =<< readDhallConfig "../dhall-configs/dev/beckn-transport.dhall"
  let loggerRt = getEulerLoggerRuntime (Just "Test_Transport_flow") defaultTestLoggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlowR flowRt appEnv $ withLogTag tag flow
