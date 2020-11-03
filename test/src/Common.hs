module Common where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
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
      BL.toStrict (responseBody response)
        `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure ("Expected " <> B.toString expectedMessage <> " error.")
