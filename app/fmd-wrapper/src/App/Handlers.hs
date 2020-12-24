module App.Handlers where

import App.Types
import Beckn.Types.FMD.API.Cancel (CancelAPI)
import Beckn.Types.FMD.API.Confirm (ConfirmAPI)
import Beckn.Types.FMD.API.Init (InitAPI)
import Beckn.Types.FMD.API.Search (SearchAPI)
import Beckn.Types.FMD.API.Select (SelectAPI)
import Beckn.Types.FMD.API.Status (StatusAPI)
import Beckn.Types.FMD.API.Track (TrackAPI)
import Beckn.Types.FMD.API.Update (UpdateAPI)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import EulerHS.Prelude
import qualified Product.APIMapper as API
import Servant
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
