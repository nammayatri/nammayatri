module External.Gateway.Api where

import qualified Beckn.Types.API.Confirm as Confirm
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Types as API
import Servant.Client

confirm :: BaseUrl -> Confirm.ConfirmReq -> L.Flow Confirm.ConfirmRes
confirm url req = do
  res <- L.callAPI url $ API.confirm req
  whenLeft res $ \err ->
    L.logError "error occurred while confirm: " (show err)
  undefined

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "",
      baseUrlPort = 443,
      baseUrlPath = ""
    }
