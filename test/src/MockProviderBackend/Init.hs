module MockProviderBackend.Init where

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
  describe "Mock Provider Backend Init Api" $
    it "should return valid ack response" do
      ctx <- buildContext "init" "dummy-txn-id" Nothing Nothing
      let quoteId = "dummy-quote-id"
          initReq = buildFMDInitReq ctx quoteId
      initiateInitRes <- runClient providerClientEnv $ initFlow mockProviderApiKey initReq
      initiateInitRes `shouldSatisfy` isRight
