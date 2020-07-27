module App.Handlers where

import App.Types
import Beckn.Types.API.Search (SearchReq)
import Beckn.Types.Common (AckResponse (..))
import Beckn.Utils.Servant.Auth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant
import Utils.Auth

type WrapperAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> ProviderSearchAPI
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

type ProviderSearchAPI =
  "search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

fmdWrapperBackendServer :: V.Key (HashMap Text Text) -> FlowServer WrapperAPI
fmdWrapperBackendServer _key =
  pure "FMD wrapper backend is UP"
    :<|> searchFlow

searchFlow :: FlowServer ProviderSearchAPI
searchFlow = P.search
