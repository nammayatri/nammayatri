module App.Handlers where

import App.Types (FlowServer)
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Status
import Beckn.Types.FMD.API.Update
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import EulerHS.Prelude
import qualified Product.Cancel as P
import qualified Product.Confirm as P
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import qualified Product.Status as P
import qualified Product.Update as P
import Servant
import Utils.Auth

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI VerifyAPIKey
           :<|> SelectAPI VerifyAPIKey
           :<|> InitAPI VerifyAPIKey
           :<|> ConfirmAPI VerifyAPIKey
           :<|> StatusAPI VerifyAPIKey
           :<|> CancelAPI VerifyAPIKey
           :<|> UpdateAPI VerifyAPIKey
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

mockProviderBackendServer :: FlowServer ProviderAPI
mockProviderBackendServer =
  pure "Mock provider backend is UP"
    :<|> searchFlow
    :<|> selectFlow
    :<|> initFlow
    :<|> confirmFlow
    :<|> statusFlow
    :<|> cancelFlow
    :<|> updateFlow

searchFlow :: FlowServer (SearchAPI VerifyAPIKey)
searchFlow = HttpSig.withBecknAuth P.search lookup :<|> P.search

selectFlow :: FlowServer (SelectAPI VerifyAPIKey)
selectFlow = HttpSig.withBecknAuth P.select lookup :<|> P.select

initFlow :: FlowServer (InitAPI VerifyAPIKey)
initFlow = HttpSig.withBecknAuth P.init lookup :<|> P.init

confirmFlow :: FlowServer (ConfirmAPI VerifyAPIKey)
confirmFlow = HttpSig.withBecknAuth P.confirm lookup :<|> P.confirm

statusFlow :: FlowServer (StatusAPI VerifyAPIKey)
statusFlow = HttpSig.withBecknAuth P.status lookup :<|> P.status

cancelFlow :: FlowServer (CancelAPI VerifyAPIKey)
cancelFlow = HttpSig.withBecknAuth P.cancel lookup :<|> P.cancel

updateFlow :: FlowServer (UpdateAPI VerifyAPIKey)
updateFlow = HttpSig.withBecknAuth P.update lookup :<|> P.update
