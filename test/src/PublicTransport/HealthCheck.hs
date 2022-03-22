module PublicTransport.HealthCheck where

import Beckn.Prelude
import Common
import HSpec
import PublicTransport.Common (callMockPublicTransportBpp, callPublicTransportBap)
import Servant hiding (Context)
import Servant.Client

type HealthCheckAPI = Get '[JSON] Text

healthCheckClientM :: ClientM Text
healthCheckClientM = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = describe "Testing healthcheck" $
  it "BAP, BPP, gateway servers should be up" $ do
    gwResult <- callGateway healthCheckClientM
    gwResult `shouldBe` "Gateway is UP"
    bapResult <- callPublicTransportBap healthCheckClientM
    bapResult `shouldBe` "App is UP"
    mockBppResult <- callMockPublicTransportBpp healthCheckClientM
    mockBppResult `shouldBe` "Mock is up!"
