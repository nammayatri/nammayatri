module MockProviderBackend.Confirm where

import Data.Time
import EulerHS.Prelude
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
      now <- getCurrentTime
      let ctx = buildContext "confirm" "dummy-txn-id" now
          confirmReq = buildFMDConfirmReq ctx
      initiateConfirmRes <- runClient providerClientEnv $ confirmFlow mockProviderApiKey confirmReq
      initiateConfirmRes `shouldSatisfy` isRight
