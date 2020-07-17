module App.Handlers where

import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant
import Utils.Auth

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

type SearchAPI = Search.SearchAPI VerifyAPIKey

mockProviderBackendServer :: V.Key (HashMap Text Text) -> FlowServerR r ProviderAPI
mockProviderBackendServer _key =
  pure "Mock provider backend is UP"
    :<|> searchFlow

searchFlow :: FlowServerR r SearchAPI
searchFlow = P.search
