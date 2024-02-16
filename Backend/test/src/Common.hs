{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Common where

import qualified API.UI.Quote as AbeQuoteAPI
import qualified "rider-app" API.UI.Search as AppBESearch
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX
import qualified "rider-app" Domain.Types.SearchRequest as BSearchRequest
import EulerHS.Prelude
import Kernel.Types.Base64
import Kernel.Types.Id
import Kernel.Types.Version
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Network.HTTP.Types.Status
import Servant.Client
import Test.Hspec hiding (context)
import Utils (defaultManager, runClient')

getAppBaseUrl :: BaseUrl
getAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8013,
      baseUrlPath = "/v2"
    }

appBackendClientEnv :: ClientEnv
appBackendClientEnv = mkClientEnv defaultManager getAppBaseUrl

callRiderApp :: (Show a) => ClientM a -> IO a
callRiderApp = runClient' appBackendClientEnv

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  ClientM AppBESearch.SearchResp
searchServices = client (Proxy :: Proxy AppBESearch.API)

getQuotes :: Id BSearchRequest.SearchRequest -> Text -> ClientM AbeQuoteAPI.GetQuotesRes
getQuotes = client (Proxy :: Proxy AbeQuoteAPI.API)

gatewayBaseUrl :: BaseUrl
gatewayBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8015,
      baseUrlPath = "/v1"
    }

gatewayClientEnv :: ClientEnv
gatewayClientEnv = mkClientEnv defaultManager gatewayBaseUrl

callGateway :: (Show a) => ClientM a -> IO a
callGateway = runClient' gatewayClientEnv

verifyError :: Int -> B.ByteString -> Either ClientError a -> IO ()
verifyError expectedCode expectedMessage serverResponse = do
  case serverResponse of
    Left (FailureResponse _ response) -> do
      statusCode (responseStatusCode response) `shouldBe` expectedCode
      BL.toStrict (responseBody response)
        `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure ("Expected " <> B.toString expectedMessage <> " error.")

privateKey :: Base64
privateKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="

signRequest :: ToJSON req => req -> POSIXTime -> Text -> Text -> ByteString
signRequest req now orgId keyId =
  let body = BL.toStrict $ J.encode req
      bodyHash = HttpSig.becknSignatureHash body
      headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      params = HttpSig.mkSignatureParams orgId keyId now 600 HttpSig.Ed25519
      signature = fromJust $ HttpSig.sign privateKey params bodyHash headers
   in HttpSig.encode $ HttpSig.SignaturePayload signature params
