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
import qualified Data.Vault.Lazy as V
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

fmdWrapperBackendServer :: V.Key (HashMap Text Text) -> FlowServer WrapperAPI
fmdWrapperBackendServer _key =
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
searchFlow = API.search

selectFlow :: FlowServer (SelectAPI VerifyAPIKey)
selectFlow = API.select

initFlow :: FlowServer (InitAPI VerifyAPIKey)
initFlow = API.init

confirmFlow :: FlowServer (ConfirmAPI VerifyAPIKey)
confirmFlow = API.confirm

statusFlow :: FlowServer (StatusAPI VerifyAPIKey)
statusFlow = API.status

trackFlow :: FlowServer (TrackAPI VerifyAPIKey)
trackFlow = API.track

cancelFlow :: FlowServer (CancelAPI VerifyAPIKey)
cancelFlow = API.cancel

updateFlow :: FlowServer (UpdateAPI VerifyAPIKey)
updateFlow = API.update
