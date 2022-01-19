module App.Routes where

import App.Types (FlowServer)
import Beckn.Types.App (MandatoryQueryParam)
import Beckn.Types.Core.Ack
import Beckn.Types.Registry.Routes (LookupAPI)
import Domain.Subscriber
import EulerHS.Prelude
import qualified Flow.Lookup as Flow
import Servant

type RegistryAPI =
  LookupAPI
    :<|> CreateAPI
    :<|> DeleteAPI

registryFlow :: FlowServer RegistryAPI
registryFlow = lookupFlow :<|> Flow.create :<|> Flow.delete

registryAPI :: Proxy RegistryAPI
registryAPI = Proxy

lookupFlow :: FlowServer LookupAPI
lookupFlow = Flow.lookup

type CreateAPI =
  "create"
    :> ReqBody '[JSON] Subscriber
    :> Post '[JSON] AckResponse

type DeleteAPI =
  "delete"
    :> MandatoryQueryParam "unique_key_id" Text
    :> MandatoryQueryParam "subscriber_id" Text
    :> Post '[JSON] AckResponse
