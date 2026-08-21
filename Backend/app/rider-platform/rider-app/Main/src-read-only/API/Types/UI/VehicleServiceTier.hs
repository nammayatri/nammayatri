{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleServiceTier where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data VehicleServiceTierAPIEntity = VehicleServiceTierAPIEntity {serviceTierName :: Kernel.Prelude.Text, serviceTierType :: Domain.Types.ServiceTierType.ServiceTierType, vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
