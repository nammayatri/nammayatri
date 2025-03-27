{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleInfo where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.VehicleInfo
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

newtype UpdateVehicleInfoReq = UpdateVehicleInfoReq {newInfo :: [Domain.Types.VehicleInfo.VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
