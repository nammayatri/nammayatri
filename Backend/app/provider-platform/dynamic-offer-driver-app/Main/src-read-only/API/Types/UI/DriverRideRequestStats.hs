{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverRideRequestStats where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverRideRequestStatsRes = DriverRideRequestStatsRes
  { acceptedRequests :: Kernel.Prelude.Int,
    lastAcceptedRequestAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastRequestAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    pulledRequests :: Kernel.Prelude.Int,
    rejectedRequests :: Kernel.Prelude.Int,
    totalRequests :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
