{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.PriceBreakup where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Types.Common
import qualified Domain.Types.Ride



data MeterRidePriceReq
    = MeterRidePriceReq {locationUpdates :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data MeterRidePriceRes
    = MeterRidePriceRes {distance :: Kernel.Types.Common.HighPrecMeters,
                         fare :: Kernel.Types.Common.HighPrecMoney,
                         status :: Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus,
                         tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



