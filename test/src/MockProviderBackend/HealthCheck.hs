module MockProviderBackend.HealthCheck where

import Data.Text.Encoding as DT
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockProviderBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec
import Utils

type HealthCheckAPI = Get '[JSON] Text

healthCheckBackendC :: ClientM Text
healthCheckBackendC = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = do
  mockProviderManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockProviderManager mockProviderBaseUrl
      loggerCfg = getLoggerCfg "mock-provider-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock Provider Backend health check API" $
      it "Testing health check API" $
        \_flowRt ->
          do
            mProviderResult <- runClient appClientEnv healthCheckBackendC
            mProviderResult `shouldBe` Right (DT.decodeUtf8 "Mock provider backend is UP")
