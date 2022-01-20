module FmdWrapper.Flow.Confirm where

import Beckn.Types.Core.Ack (AckResponse)
import qualified Beckn.Types.Core.ReqTypes as API
import Common (signRequest)
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude hiding (id)
import Fmd (buildContext, fmdWrapperBaseUrl)
import FmdWrapper.Common (assertAck, withNewUUID)
import qualified FmdWrapper.Fixtures as Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, (:>))
import Servant.Client (ClientEnv, ClientError, client, mkClientEnv)
import Test.Hspec hiding (example)
import qualified "fmd-wrapper" Types.Beckn.API.Confirm as ConfirmAPI
import "fmd-wrapper" Types.Beckn.Context (Action (..))
import Utils (runClient)

runConfirm :: ClientEnv -> Text -> API.BecknReq ConfirmAPI.OrderObject -> IO (Either ClientError AckResponse)
runConfirm clientEnv orgId confirmReq = do
  now <- getPOSIXTime
  let signature = decodeUtf8 $ signRequest confirmReq now orgId (orgId <> "-key")
  let confirmAPI = Proxy :: Proxy (Header "Authorization" Text :> ConfirmAPI.ConfirmAPI)
  runClient clientEnv $ client confirmAPI (Just signature) confirmReq

successfulConfirm :: ClientEnv -> IO ()
successfulConfirm clientEnv =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext CONFIRM transactionId
    let confirmReq = API.BecknReq ctx Fixtures.confirmOrderObject
    response <- runConfirm clientEnv "fmd-test-app" confirmReq
    assertAck response

-- this test is redundant because of strict types
-- confirmWithoutPaymentTransactionId :: ClientEnv -> IO ()
-- confirmWithoutPaymentTransactionId clientEnv =
--   withNewUUID $ \transactionId -> do
--     ctx <- buildContext CONFIRM transactionId
--     let orderWithoutTrnsxnId = confirmOrder & #order . #payment . #params . #transaction_id .~ Nothing
--     let confirmReq = API.BecknReq ctx orderWithoutTrnsxnId
--     response <- runConfirm clientEnv "fmd-test-app" confirmReq
--     verifyError 400 "TXN_ID_NOT_PRESENT" response

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv appManager fmdWrapperBaseUrl
  describe "Confirm API" do
    it "Successful confirm" $ successfulConfirm appClientEnv

-- this test is redundant because of strict types
-- it "Fail if payment transaction id not found" $ confirmWithoutPaymentTransactionId appClientEnv
