{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AutoAcceptRide where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data AutoAcceptRideReq = AutoAcceptRideReq {acknowledged :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
