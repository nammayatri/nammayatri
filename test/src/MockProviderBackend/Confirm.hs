module MockProviderBackend.Confirm where

import Common
import Data.Time.Clock.POSIX
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
      now <- getPOSIXTime
      let signature = decodeUtf8 $ signRequest confirmReq now mockProviderSelfId mockProviderApiKey
      initiateConfirmRes <- runClient providerClientEnv $ confirmFlow (Just signature) confirmReq
      initiateConfirmRes `shouldSatisfy` isRight
