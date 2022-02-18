module API.Beckn.Types where

import Beckn.Utils.Servant.SignatureAuth
import Core.Spec.API.OnConfirm
import Core.Spec.API.OnSearch
import Core.Spec.API.OnStatus
import Servant

type API =
  "beckn"
    :> SignatureAuth "Authorization"
    :> ( OnSearchAPI
           :<|> OnConfirmAPI
           :<|> OnStatusAPI
       )
