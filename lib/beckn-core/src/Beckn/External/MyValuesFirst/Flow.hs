module Beckn.External.MyValuesFirst.Flow where

import qualified Beckn.External.MyValuesFirst.API as API
import Beckn.External.MyValuesFirst.Types
import EulerHS.Language
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client

submitSms :: BaseUrl -> SubmitSms -> Flow (Either Text ())
submitSms url params = do
  res <- callAPI url $ API.submitSms params
  whenRight res $ \_ ->
    L.logInfo "SMS" $ "Submitted sms successfully to " <> show (_to params)
  return $ first show res

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "http.myvfirst.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }
