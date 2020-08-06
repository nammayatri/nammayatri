module MockAppBackend.HealthCheck where

import Data.Text.Encoding as DT
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockAppBackend.Fixtures
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
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
      loggerCfg = getLoggerConfig "mock-app-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock App Backend health check API" $
      it "Testing health check API" $
        \_flowRt ->
          do
            mAppResult <- runClient appClientEnv healthCheckBackendC
            mAppResult `shouldBe` Right (DT.decodeUtf8 "Mock app backend is UP")
