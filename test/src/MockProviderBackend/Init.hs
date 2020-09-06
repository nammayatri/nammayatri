module MockProviderBackend.Init where

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
  describe "Mock Provider Backend Init Api" $
    it "should return valid ack response" do
      now <- getCurrentTime
      let ctx = buildContext "init" "dummy-txn-id" now
          quoteId = "dummy-quote-id"
          initReq = buildFMDInitReq ctx quoteId
      initiateInitRes <- runClient providerClientEnv $ initFlow mockProviderApiKey initReq
      initiateInitRes `shouldSatisfy` isRight
