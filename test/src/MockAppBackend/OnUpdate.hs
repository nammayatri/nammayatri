module MockAppBackend.OnUpdate where

import Common
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Fmd
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
  describe "Mock App Backend OnUpdate Api" $
    it "should return valid ack response" do
      ctx <- buildContext "on_update" "dummy-txn-id" Nothing Nothing
      let onUpdateReq = buildOnUpdateReq ctx
      now <- getPOSIXTime
      let signature = decodeUtf8 $ signRequest onUpdateReq now mockAppSelfId mockAppApiKey
      eitherUpdateCbRes <- runClient appClientEnv $ onUpdateFlow (Just signature) onUpdateReq
      eitherUpdateCbRes `shouldSatisfy` isRight
