module MockAppBackend.TriggerSearch where

import EulerHS.Prelude
import MockAppBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
  describe "Mock App Backend Search Api" $
    it "should return valid ack response" do
      initiateSearchRes <- runClient appClientEnv $ triggerSearchReq MockAppTrigger.NoSearchResult
      initiateSearchRes `shouldSatisfy` isRight
