module API.Beckn (API, handler) where

import qualified API.Beckn.OnConfirm as OnConfirm
import qualified API.Beckn.OnInit as OnInit
import qualified API.Beckn.OnSearch as OnSearch
import qualified API.Beckn.OnSelect as OnSelect
import qualified API.Beckn.OnTrack as OnTrack
import qualified API.Beckn.OnUpdate as OnUpdate
import App.Types
import Beckn.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "cab" :> "v1" :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnSelect.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnUpdate.API
           :<|> OnTrack.API
       )

handler :: FlowServer API
handler auth =
  OnSearch.handler auth
    :<|> OnSelect.handler auth
    :<|> OnInit.handler auth
    :<|> OnConfirm.handler auth
    :<|> OnUpdate.handler auth
    :<|> OnTrack.handler auth
