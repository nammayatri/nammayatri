module MockProviderBackend.Select where

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
  describe "Mock Provider Backend Select Api" $
    it "should return valid ack response" do
      ctx <- buildContext "select" "dummy-txn-id" Nothing Nothing
      let selectReq = buildFMDSelectReq ctx
      initiateSelectRes <- runClient providerClientEnv $ selectFlow selectReq
      initiateSelectRes `shouldSatisfy` isRight
