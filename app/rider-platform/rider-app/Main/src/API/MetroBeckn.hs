module API.MetroBeckn (API, handler) where

import qualified API.MetroBeckn.OnSearch as OnSearch
import Environment
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "metro" :> "v1"
    :> SignatureAuth "Authorization"
    :> OnSearch.API

handler :: FlowServer API
handler = OnSearch.handler
