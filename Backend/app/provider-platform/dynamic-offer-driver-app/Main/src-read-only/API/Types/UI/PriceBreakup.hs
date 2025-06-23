{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PriceBreakup where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data MeterRidePriceReq = MeterRidePriceReq {locationUpdates :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MeterRidePriceRes = MeterRidePriceRes
  { distance :: Kernel.Types.Common.HighPrecMeters,
    fare :: Kernel.Types.Common.HighPrecMoney,
    status :: Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
