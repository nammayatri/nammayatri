{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSInternal where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data NotifyBusApproachingReq = NotifyBusApproachingReq
  { distanceMeters :: Kernel.Prelude.Double,
    etaSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    gtfsId :: Kernel.Prelude.Text,
    routeId :: Kernel.Prelude.Text,
    thresholdType :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
