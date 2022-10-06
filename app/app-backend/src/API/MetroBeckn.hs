module API.MetroBeckn (API, handler) where

import qualified API.MetroBeckn.OnSearch as OnSearch
import Beckn.Utils.Servant.SignatureAuth
import Environment
import Servant hiding (throwError)

type API =
  "metro" :> "v1"
    :> SignatureAuth "Authorization"
    :> OnSearch.API

handler :: FlowServer API
handler = OnSearch.handler
