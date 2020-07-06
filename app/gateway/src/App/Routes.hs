{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import Beckn.Types.App (FlowServer)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant

type GatewayAPI =
  "v1" :> Get '[JSON] Text

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: V.Key (HashMap Text Text) -> FlowServer GatewayAPI
gatewayServer key =
  pure "Gateway is UP"
