module HealthCheck where

import Data.Text.Encoding as DT
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

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  tbeManager <- runIO $ Client.newManager tlsManagerSettings
  gatewayManager <- runIO $ Client.newManager tlsManagerSettings
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  mockProviderManager <- runIO $ Client.newManager tlsManagerSettings
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
      gatewayClientEnv = mkClientEnv gatewayManager $ appBaseUrl {baseUrlPort = 8015}
      mockAppClientEnv = mkClientEnv mockAppManager $ appBaseUrl {baseUrlPort = 8016}
      mockProviderClientEnv = mkClientEnv mockProviderManager $ appBaseUrl {baseUrlPort = 8017}
      loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/app-backend-healthcheck-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Testing App Backend APIs" $
      it "Testing health check API" $
        \_flowRt ->
          hspec $
            it "Health Check API should return success" $
              do
                appResult <- runClient appClientEnv healthCheckBackendC
                appResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
                tbeResult <- runClient tbeClientEnv healthCheckBackendC
                tbeResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
                gwResult <- runClient gatewayClientEnv healthCheckBackendC
                gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
                mAppResult <- runClient mockAppClientEnv healthCheckBackendC
                mAppResult `shouldBe` Right (DT.decodeUtf8 "Mock app backend is UP")
                mProviderResult <- runClient mockProviderClientEnv healthCheckBackendC
                mProviderResult `shouldBe` Right (DT.decodeUtf8 "Mock provider backend is UP")
