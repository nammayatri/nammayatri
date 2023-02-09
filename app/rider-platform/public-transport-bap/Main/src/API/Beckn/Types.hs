module API.Beckn.Types where

import Beckn.Spec.API.OnCancel
import Beckn.Spec.API.OnConfirm
import Beckn.Spec.API.OnSearch
import Beckn.Spec.API.OnStatus
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  "beckn"
    :> SignatureAuth "Authorization"
    :> ( OnSearchAPI
           :<|> OnConfirmAPI
           :<|> OnStatusAPI
           :<|> OnCancelAPI
       )
