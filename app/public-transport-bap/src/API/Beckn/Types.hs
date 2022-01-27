module API.Beckn.Types where

import qualified API.Beckn.OnSearch.Types as OnSearch -- FIXME: should be Core.Spec.OnSearch.API?
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.Spec.OnConfirm.API as OnConfirm
import Servant

type API =
  "beckn"
    :> SignatureAuth "Authorization"
    :> (OnSearch.API :<|> OnConfirm.API)
