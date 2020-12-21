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
           :<|> SearchAPI VerifyAPIKey
           :<|> SelectAPI VerifyAPIKey
           :<|> InitAPI VerifyAPIKey
           :<|> ConfirmAPI VerifyAPIKey
           :<|> StatusAPI VerifyAPIKey
           :<|> TrackAPI VerifyAPIKey
           :<|> CancelAPI VerifyAPIKey
           :<|> UpdateAPI VerifyAPIKey
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

fmdWrapperBackendServer :: FlowServer WrapperAPI
fmdWrapperBackendServer =
  pure "FMD wrapper backend is UP"
    :<|> searchFlow
    :<|> selectFlow
    :<|> initFlow
    :<|> confirmFlow
    :<|> statusFlow
    :<|> trackFlow
    :<|> cancelFlow
    :<|> updateFlow

searchFlow :: FlowServer (SearchAPI VerifyAPIKey)
searchFlow = HttpSig.withBecknAuth API.search lookup :<|> API.search

selectFlow :: FlowServer (SelectAPI VerifyAPIKey)
selectFlow = HttpSig.withBecknAuth API.select lookup :<|> API.select

initFlow :: FlowServer (InitAPI VerifyAPIKey)
initFlow = HttpSig.withBecknAuth API.init lookup :<|> API.init

confirmFlow :: FlowServer (ConfirmAPI VerifyAPIKey)
confirmFlow = HttpSig.withBecknAuth API.confirm lookup :<|> API.confirm

statusFlow :: FlowServer (StatusAPI VerifyAPIKey)
statusFlow = HttpSig.withBecknAuth API.status lookup :<|> API.status

trackFlow :: FlowServer (TrackAPI VerifyAPIKey)
trackFlow = HttpSig.withBecknAuth API.track lookup :<|> API.track

cancelFlow :: FlowServer (CancelAPI VerifyAPIKey)
cancelFlow = HttpSig.withBecknAuth API.cancel lookup :<|> API.cancel

updateFlow :: FlowServer (UpdateAPI VerifyAPIKey)
updateFlow = HttpSig.withBecknAuth API.update lookup :<|> API.update
