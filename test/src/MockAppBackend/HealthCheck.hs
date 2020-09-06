module MockAppBackend.HealthCheck where

import Data.Text.Encoding as DT
import EulerHS.Prelude
import MockAppBackend.Fixtures
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
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
  describe "Mock App Backend health check API" $
    it "Testing health check API" do
      do
        mAppResult <- runClient appClientEnv healthCheckBackendC
        mAppResult `shouldBe` Right (DT.decodeUtf8 "Mock app backend is UP")
