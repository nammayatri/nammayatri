{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Invoice where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data FareBreakup = FareBreakup {price :: Data.Text.Text, title :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data InvoiceRes = InvoiceRes
  { chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    date :: Kernel.Prelude.UTCTime,
    destination :: Data.Text.Text,
    driverName :: Data.Text.Text,
    faresList :: [API.Types.UI.Invoice.FareBreakup],
    rideEndTime :: Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.UTCTime,
    shortRideId :: Data.Text.Text,
    source :: Data.Text.Text,
    totalAmount :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
