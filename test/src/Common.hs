module Common where

import Beckn.Utils.Registry (decodeKey)
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Time.Clock.POSIX
import EulerHS.Prelude
import Network.HTTP.Types.Status
import Servant.Client
import Test.Hspec hiding (context)

gatewayBaseUrl :: BaseUrl
gatewayBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8015,
      baseUrlPath = "/v1"
    }

verifyError :: Int -> B.ByteString -> Either ClientError a -> IO ()
verifyError expectedCode expectedMessage serverResponse = do
  case serverResponse of
    Left (FailureResponse _ response) -> do
      statusCode (responseStatusCode response) `shouldBe` expectedCode
    -- TODO: Fix this test
    -- BL.toStrict (responseBody response)
    --   `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure ("Expected " <> B.toString expectedMessage <> " error.")

privateKey :: ByteString
privateKey = fromJust $ decodeKey "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="

signRequest :: ToJSON req => req -> POSIXTime -> Text -> Text -> ByteString
signRequest req now orgId keyId =
  let body = BL.toStrict $ J.encode req
      headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      params = HttpSig.mkSignatureParams orgId keyId now 600 HttpSig.Ed25519
      signature = fromJust $ HttpSig.sign privateKey params body headers
   in HttpSig.encode $ HttpSig.SignaturePayload signature params
