module HealthCheck where

import qualified "app-backend" App as AppBE
import qualified "beckn-transport" App as TransporterBE
import Data.Text.Encoding as DT
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Types as T
import Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec

type HealthCheckAPI = Get '[JSON] Text

healthCheckBackendC :: ClientM Text
healthCheckBackendC = client (Proxy :: Proxy HealthCheckAPI)

startServers :: IO (ThreadId, ThreadId)
startServers = do
  appTid <- forkIO AppBE.runAppBackend
  tbeTid <- forkIO TransporterBE.runTransporterBackendApp
  return (appTid, tbeTid)

withBecknServers :: IO () -> IO ()
withBecknServers action = do
  bracket
    startServers
    (\(appTid, tbeTid) -> killThread appTid >> killThread tbeTid)
    (const $ threadDelay 100000 >> action)

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

spec :: Spec
spec = do
  reqHeadersKey <- runIO V.newKey
  appManager <- runIO $ Client.newManager tlsManagerSettings
  tbeManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
      transporterBaseUrl = appBaseUrl {baseUrlPort = 8014}
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv tbeManager transporterBaseUrl
      loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/app-backend-healthcheck-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg))
    $ describe "Testing App Backend APIs"
    $ it "Testing health check API"
    $ \flowRt -> do
      hspec
        $ around_ withBecknServers
        $ it "Health Check API should return success"
        $ do
          result <- runClient appClientEnv healthCheckBackendC
          result `shouldBe` Right (DT.decodeUtf8 "App is UP")
          result <- runClient tbeClientEnv healthCheckBackendC
          result `shouldBe` Right (DT.decodeUtf8 "App is UP")
