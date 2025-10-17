{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverToDriverDirectCalling where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverGetnumberResp = DriverGetnumberResp {mobileNumber :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
