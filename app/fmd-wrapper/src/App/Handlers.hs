module App.Handlers where

import App.Types
import Beckn.Types.API.Search (SearchAPI)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant
import Utils.Auth

type WrapperAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI VerifyAPIKey
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

fmdWrapperBackendServer :: V.Key (HashMap Text Text) -> FlowServer WrapperAPI
fmdWrapperBackendServer _key =
  pure "FMD wrapper backend is UP"
    :<|> searchFlow

searchFlow :: FlowServer (SearchAPI VerifyAPIKey)
searchFlow = P.search
