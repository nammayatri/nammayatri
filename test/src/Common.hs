module Common where

import "app-backend" App.Routes as AbeRoutes
import Beckn.Types.Base64
import Beckn.Types.Id
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Time.Clock.POSIX
import qualified "app-backend" Domain.Types.SearchRequest as BSearchRequest
import EulerHS.Prelude
import Network.HTTP.Types.Status
import Servant.Client
import Test.Hspec hiding (context)
import qualified Types.API.Quote as QuoteAPI
import qualified "app-backend" Types.API.Search as AppBESearch
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

callAppBackend :: (Show a) => ClientM a -> IO a
callAppBackend = runClient' appBackendClientEnv

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  ClientM AppBESearch.SearchRes
searchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

getQuotes :: Id BSearchRequest.SearchRequest -> Text -> ClientM QuoteAPI.GetQuotesRes
getQuotes = client (Proxy :: Proxy AbeRoutes.QuoteAPI)

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
privateKey = "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="

signRequest :: ToJSON req => req -> POSIXTime -> Text -> Text -> ByteString
signRequest req now orgId keyId =
  let body = BL.toStrict $ J.encode req
      bodyHash = HttpSig.becknSignatureHash body
      headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      params = HttpSig.mkSignatureParams orgId keyId now 600 HttpSig.Ed25519
      signature = fromJust $ HttpSig.sign privateKey params bodyHash headers
   in HttpSig.encode $ HttpSig.SignaturePayload signature params
