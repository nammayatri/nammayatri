{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.MeterRide where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.External.Maps.Types
import qualified Domain.Types.Location
import qualified Kernel.Types.Common
import qualified Data.Text



data MeterRideAddDestinationReq
    = MeterRideAddDestinationReq {currentLatLong :: Kernel.External.Maps.Types.LatLong,
                                  destinationLatLong :: Kernel.External.Maps.Types.LatLong,
                                  destinationLocation :: Domain.Types.Location.LocationAddress}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data MeterRideAddDestinationResp
    = MeterRideAddDestinationResp {estimatedDistance :: Kernel.Types.Common.Meters, estimatedFare :: Kernel.Types.Common.HighPrecMoney}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data SendRecietRequest
    = SendRecietRequest {customerMobileCountryCode :: Data.Text.Text, customerMobileNumber :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



