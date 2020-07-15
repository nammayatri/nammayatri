module App.Handlers where

import Beckn.Types.App
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import "beckn-gateway" External.Provider.Routes
import qualified Product.Search as P
import Servant

mockProviderBackendServer :: V.Key (HashMap Text Text) -> FlowServerR r ProviderAPI
mockProviderBackendServer _key =
  pure "Mock provider backend is UP"
    :<|> searchFlow

searchFlow :: FlowServerR r SearchAPI
searchFlow = P.search
