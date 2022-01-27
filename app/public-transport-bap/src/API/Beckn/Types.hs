module API.Beckn.Types where

import qualified API.Beckn.OnSearch.Types as OnSearch
import Beckn.Utils.Servant.SignatureAuth
import Servant

type API =
  "beckn"
    :> SignatureAuth "Authorization"
    :> OnSearch.API