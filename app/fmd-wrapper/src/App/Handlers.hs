module App.Handlers where

import App.Types
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
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
import Utils.Auth

type WrapperAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> HttpSig.SignatureAuth "Authorization" :> SearchAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> SelectAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> InitAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> ConfirmAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> StatusAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> TrackAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> CancelAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> UpdateAPI
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

fmdWrapperBackendServer :: FlowServer WrapperAPI
fmdWrapperBackendServer =
  pure "FMD wrapper backend is UP"
    :<|> HttpSig.withBecknAuth API.search lookup
    :<|> HttpSig.withBecknAuth API.select lookup
    :<|> HttpSig.withBecknAuth API.init lookup
    :<|> HttpSig.withBecknAuth API.confirm lookup
    :<|> HttpSig.withBecknAuth API.status lookup
    :<|> HttpSig.withBecknAuth API.track lookup
    :<|> HttpSig.withBecknAuth API.cancel lookup
    :<|> HttpSig.withBecknAuth API.update lookup
