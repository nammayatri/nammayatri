{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Merchant where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data SwitchGateWayAndRegistry = SwitchGateWayAndRegistry {gatewayType :: API.Types.UI.Merchant.ToggleToType, registryType :: API.Types.UI.Merchant.ToggleToType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ToggleToType
  = ONDC
  | JUSPAY
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
