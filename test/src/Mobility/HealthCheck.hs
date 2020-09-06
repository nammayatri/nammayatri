module Mobility.HealthCheck where

import Data.Text.Encoding as DT
import EulerHS.Prelude
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
  tbeManager <- runIO $ Client.newManager tlsManagerSettings
  gatewayManager <- runIO $ Client.newManager tlsManagerSettings
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
