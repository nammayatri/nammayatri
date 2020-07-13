-- | API of provider which the gateway expects.
--
-- Each provider backend should implement a server for this API.
module External.Provider.Routes where

import qualified Beckn.Types.API.Search as Search
import EulerHS.Prelude
import Servant

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

type SearchAPI =
  Search.SearchAPI
