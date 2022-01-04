{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Common where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Control.Lens (Setter', _Just)
import Data.Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec hiding (context)
import "fmd-wrapper" Types.Beckn.API.Search (SearchIntent)
import "fmd-wrapper" Types.Beckn.Gps (Gps)

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
assertAck = (`shouldSatisfy` isRight)

withNewUUID :: (Text -> IO ()) -> IO ()
withNewUUID action = do
  uuid <- UUID.nextUUID
  maybe
    (expectationFailure "Could not generate UUID.")
    (action . UUID.toText)
    uuid

setIntentPickupGps :: Setter' (BecknReq SearchIntent) (Maybe Gps)
setIntentPickupGps = #message . #intent . #fulfillment . _Just . #start . _Just . #location . _Just . #gps

setIntentDropGps :: Setter' (BecknReq SearchIntent) (Maybe Gps)
setIntentDropGps = #message . #intent . #fulfillment . _Just . #end . _Just . #location . _Just . #gps
