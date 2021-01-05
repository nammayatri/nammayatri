module MockProviderBackend.Init where

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
  describe "Mock Provider Backend Init Api" $
    it "should return valid ack response" do
      ctx <- buildContext "init" "dummy-txn-id" Nothing Nothing
      let quoteId = "dummy-quote-id"
          initReq = buildFMDInitReq ctx quoteId
      now <- getPOSIXTime
      let signature = decodeUtf8 $ signRequest initReq now mockProviderSelfId mockProviderApiKey
      initiateInitRes <- runClient providerClientEnv $ initFlow (Just signature) initReq
      initiateInitRes `shouldSatisfy` isRight
