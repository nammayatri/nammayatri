module App.Handlers where

import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.FMD.API.Init (InitReq)
import Beckn.Types.FMD.API.Search (SearchReq)
import Beckn.Types.FMD.API.Select (SelectReq)
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import Servant
import Utils.Auth

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> ProviderSearchAPI
           :<|> ProviderSelectAPI
           :<|> ProviderInitAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

type ProviderSearchAPI =
  "search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

type ProviderSelectAPI =
  "select"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] AckResponse

type ProviderInitAPI =
  "init"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] AckResponse

mockProviderBackendServer :: V.Key (HashMap Text Text) -> FlowServerR r ProviderAPI
mockProviderBackendServer _key =
  pure "Mock provider backend is UP"
    :<|> searchFlow
    :<|> selectFlow
    :<|> initFlow

searchFlow :: FlowServerR r ProviderSearchAPI
searchFlow = P.search

selectFlow :: FlowServerR r ProviderSelectAPI
selectFlow = P.select

initFlow :: FlowServerR r ProviderInitAPI
initFlow = P.init
