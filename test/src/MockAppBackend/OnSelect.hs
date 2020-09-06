module MockAppBackend.OnSelect where

import Data.Time
import EulerHS.Prelude
import MockAppBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
  describe "Mock App Backend OnSelect Api" $
    it "should return valid ack response" do
      now <- getCurrentTime
      let ctx = buildContext "on_select" "dummy-txn-id" now
          onSelectReq = buildOnSelectReq ctx
      eitherSelectCbRes <- runClient appClientEnv $ onSelectFlow mockAppApiKey onSelectReq
      eitherSelectCbRes `shouldSatisfy` isRight
