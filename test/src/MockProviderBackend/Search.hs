module MockProviderBackend.Search where

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

  describe "Mock Provider Backend Search Api" $
    it "should return valid ack response" do
      now <- getCurrentTime
      let ctx = buildContext "search" "dummy-txn-id" now
          searchReq = buildFMDSearchReq ctx
      initiateSearchRes <- runClient providerClientEnv $ searchFlow mockProviderApiKey searchReq
      initiateSearchRes `shouldSatisfy` isRight
