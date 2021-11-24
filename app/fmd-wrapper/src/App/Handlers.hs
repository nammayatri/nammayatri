module App.Handlers where

import App.Types
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified Product.APIMapper as API
import Servant
import Types.Beckn.API.Cancel (CancelAPI)
import Types.Beckn.API.Confirm (ConfirmAPI)
import Types.Beckn.API.Init (InitAPI)
import Types.Beckn.API.Search (SearchAPI)
import Types.Beckn.API.Select (SelectAPI)
import Types.Beckn.API.Status (StatusAPI)
import Types.Beckn.API.Track (TrackAPI)
import Types.Beckn.API.Update (UpdateAPI)

type WrapperAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SignatureAuth "Authorization" :> SearchAPI
           :<|> SignatureAuth "Authorization" :> SelectAPI
           :<|> SignatureAuth "Authorization" :> InitAPI
           :<|> SignatureAuth "Authorization" :> ConfirmAPI
           :<|> SignatureAuth "Authorization" :> StatusAPI
           :<|> SignatureAuth "Authorization" :> TrackAPI
           :<|> SignatureAuth "Authorization" :> CancelAPI
           :<|> SignatureAuth "Authorization" :> UpdateAPI
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

fmdWrapperBackendServer :: FlowServer WrapperAPI
fmdWrapperBackendServer =
  pure "FMD wrapper backend is UP"
    :<|> API.search
    :<|> API.select
    :<|> API.init
    :<|> API.confirm
    :<|> API.status
    :<|> API.track
    :<|> API.cancel
    :<|> API.update
