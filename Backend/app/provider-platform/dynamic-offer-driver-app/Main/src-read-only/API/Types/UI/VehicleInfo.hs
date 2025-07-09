{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleInfo where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data VehicleExtraInformation = VehicleExtraInformation {rcNo :: Kernel.Prelude.Text, mediaUploaded :: Kernel.Prelude.Bool, vehicleInfo :: [VehicleInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoAPIEntity = VehicleInfoAPIEntity {questionId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, answer :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
