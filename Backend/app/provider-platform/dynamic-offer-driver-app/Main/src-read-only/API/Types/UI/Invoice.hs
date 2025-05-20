{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Invoice where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data InvoiceRes = InvoiceRes
  { date :: Kernel.Prelude.UTCTime,
    driverName :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    chargeableDistance :: Kernel.Types.Common.HighPrecMeters,
    fare :: Kernel.Prelude.Int,
    rideStartTime :: Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.UTCTime,
    shortRideId :: Kernel.Prelude.Text,
    source :: Kernel.Prelude.Text,
    destination :: Kernel.Prelude.Text,
    chargeableDistanceWithUnit :: Kernel.Types.Common.Distance
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
