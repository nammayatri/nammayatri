module MockProviderBackend.Confirm where

import EulerHS.Prelude
import Fmd
import MockProviderBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockProviderManager <- runIO $ Client.newManager tlsManagerSettings
  let providerClientEnv = mkClientEnv mockProviderManager mockProviderBaseUrl
  describe "Mock Provider Backend Confirm Api" $
    it "should return valid ack response" do
      ctx <- buildContext "confirm" "dummy-txn-id" Nothing Nothing
      let confirmReq = buildFMDConfirmReq ctx
      initiateConfirmRes <- runClient providerClientEnv $ confirmFlow confirmReq
      initiateConfirmRes `shouldSatisfy` isRight
