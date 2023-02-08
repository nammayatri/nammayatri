module API.Beckn.Types where

import Core.Spec.API.OnCancel
import Core.Spec.API.OnConfirm
import Core.Spec.API.OnSearch
import Core.Spec.API.OnStatus
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
