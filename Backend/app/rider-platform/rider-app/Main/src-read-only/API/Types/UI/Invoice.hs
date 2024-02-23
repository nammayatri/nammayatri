{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Invoice where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data FareBreakup = FareBreakup
  { price :: Kernel.Types.Common.HighPrecMoney,
    title :: Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data InvoiceRes = InvoiceRes
  { date :: Kernel.Prelude.UTCTime,
    destination :: Data.Text.Text,
    driverName :: Data.Text.Text,
    faresList :: [API.Types.UI.Invoice.FareBreakup],
    rideEndTime :: Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.UTCTime,
    shortRideId :: Data.Text.Text,
    source :: Data.Text.Text,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    vehicleNumber :: Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
