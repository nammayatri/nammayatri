module App.Routes where

import App.Types (FlowServer)
import Beckn.Types.Registry.Routes (LookupAPI)
import EulerHS.Prelude
import qualified Flow.Lookup as Flow
import Utils.Auth (LookupRegistryOrg)

type RegistryAPI = LookupAPI LookupRegistryOrg

registryFlow :: FlowServer RegistryAPI
registryFlow = lookupFlow

registryAPI :: Proxy RegistryAPI
registryAPI = Proxy

lookupFlow :: FlowServer (LookupAPI LookupRegistryOrg)
lookupFlow = Flow.lookup
