{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleDetails where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data VehicleDetailsReq = VehicleDetailsReq {make :: Data.Text.Text, model :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleMakesResp = VehicleMakesResp {makes :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleModelsReq = VehicleModelsReq {make :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleModelsResp = VehicleModelsResp {models :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
