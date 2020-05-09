module Epass.External.MyValuesFirst.Flow where

import           EulerHS.Language
import qualified EulerHS.Language                           as L
import           EulerHS.Prelude

import           Servant.Client

import qualified Epass.External.MyValuesFirst.API             as API
import           Epass.External.MyValuesFirst.Types

submitSms :: BaseUrl -> SubmitSms -> Flow (Either Text ())
submitSms url params = do
  res <- callAPI url $ API.submitSms params
  whenRight res $ \_ ->
    L.logInfo "SMS" $ "Submitted sms successfully to " <> show (_to params)
  return $ first show res

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "www.myvaluefirst.com"
    , baseUrlPort = 443
    , baseUrlPath = ""
    }
