module Mobility.HealthCheck where

import Common (getAppBaseUrl)
import Data.Text.Encoding as DT
import EulerHS.Prelude
import Mobility.Fixtures
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
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      transporterBaseUrl = getTransporterBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv appManager transporterBaseUrl
      gatewayClientEnv =
        mkClientEnv appManager $
          transporterBaseUrl
            { baseUrlPort = 8015,
              baseUrlPath = "/v1"
            }
  describe "Testing App Backend APIs" $
    it "Testing health check API" $
      hspec $
        it "Health Check API should return success" do
          appResult <- runClient appClientEnv healthCheckBackendC
          appResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          tbeResult <- runClient tbeClientEnv healthCheckBackendC
          tbeResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          gwResult <- runClient gatewayClientEnv healthCheckBackendC
          gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
