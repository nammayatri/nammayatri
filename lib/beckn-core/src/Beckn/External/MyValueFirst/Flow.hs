module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types
import Beckn.Types.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client

submitSms :: BaseUrl -> SubmitSms -> FlowR r (Either Text ())
submitSms url params = do
  res <- L.callAPI url $ API.submitSms params
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

constructOtpSms :: Text -> Text -> Text
constructOtpSms code hash =
  "<#> Your OTP is: " <> code <> "\n" <> hash
