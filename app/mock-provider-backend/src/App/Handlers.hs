module App.Handlers where

import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.FMD.API.Search (SearchReq)
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant
import Utils.Auth

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> ProviderSearchAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

type ProviderSearchAPI =
  "search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

mockProviderBackendServer :: V.Key (HashMap Text Text) -> FlowServerR r ProviderAPI
mockProviderBackendServer _key =
  pure "Mock provider backend is UP"
    :<|> searchFlow

searchFlow :: FlowServerR r ProviderSearchAPI
searchFlow = P.search
