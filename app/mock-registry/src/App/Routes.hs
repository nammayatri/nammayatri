module App.Routes where

import App.Types (FlowServer)
import Beckn.Types.Registry.Routes (LookupAPI)
import EulerHS.Prelude
import qualified Flow.Lookup as Flow

type RegistryAPI = LookupAPI

registryFlow :: FlowServer RegistryAPI
registryFlow = lookupFlow

registryAPI :: Proxy RegistryAPI
registryAPI = Proxy

lookupFlow :: FlowServer LookupAPI
lookupFlow = Flow.lookup
