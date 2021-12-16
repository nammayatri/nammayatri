module API.Beckn.Types where

import qualified API.Beckn.OnConfirm.Types as OnConfirm
import qualified API.Beckn.OnSearch.Types as OnSearch
import qualified API.Beckn.OnStatus.Types as OnStatus
import Beckn.Utils.Servant.SignatureAuth
import Servant

type API =
  "beckn"
    :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnConfirm.API
           :<|> OnStatus.API
       )
