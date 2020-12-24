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
           :<|> HttpSig.SignatureAuth "Authorization" :> SearchAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> SelectAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> InitAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> ConfirmAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> StatusAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> CancelAPI
           :<|> HttpSig.SignatureAuth "Authorization" :> UpdateAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

mockProviderBackendServer :: FlowServer ProviderAPI
mockProviderBackendServer =
  pure "Mock provider backend is UP"
    :<|> HttpSig.withBecknAuth P.search lookup
    :<|> HttpSig.withBecknAuth P.select lookup
    :<|> HttpSig.withBecknAuth P.init lookup
    :<|> HttpSig.withBecknAuth P.confirm lookup
    :<|> HttpSig.withBecknAuth P.status lookup
    :<|> HttpSig.withBecknAuth P.cancel lookup
    :<|> HttpSig.withBecknAuth P.update lookup
