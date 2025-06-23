{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleInfo where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data VehicleInfoAPIEntity = VehicleInfoAPIEntity {rcNo :: Kernel.Prelude.Text, questionId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, answer :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
