module FmdWrapper.Common where

import Beckn.Types.Core.Ack
import Data.Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec hiding (context)

data CallbackResult a = CallbackResult
  { apiKey :: Maybe Text,
    result :: a
  }
  deriving (Generic, Show, ToJSON)

fmdTestAppPort :: Port
fmdTestAppPort = 8019

fmdTestAppBaseUrl :: BaseUrl
fmdTestAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = fmdTestAppPort,
      baseUrlPath = "/v1"
    }

verifyApiKey :: Maybe Text -> IO ()
verifyApiKey apiKey = apiKey `shouldSatisfy` isJust

assertAck :: Either ClientError AckResponse -> IO ()
assertAck response =
  case response of
    Left _ -> expectationFailure $ "Received an error response: " <> show response
    Right ackResponse -> _status (_ack (_message ackResponse)) `shouldBe` ACK

withNewUUID :: (Text -> IO ()) -> IO ()
withNewUUID action = do
  uuid <- UUID.nextUUID
  maybe
    (expectationFailure "Could not generate UUID.")
    (action . UUID.toText)
    uuid
