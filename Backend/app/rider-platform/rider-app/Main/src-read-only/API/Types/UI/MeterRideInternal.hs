{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.MeterRideInternal where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Kernel.External.Maps.Types
import qualified Domain.Types.LocationAddress



data CustomerInfo
    = CustomerInfo {customerMobileCountryCode :: Data.Text.Text, customerMobileNumber :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data CustomerInfoResponse
    = CustomerInfoResponse {alreadyReferred :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, isMultipleDeviceIdExist :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data MeterRideAddDestinationReq
    = MeterRideAddDestinationReq {destinationLatLong :: Kernel.External.Maps.Types.LatLong, destinationLocation :: Domain.Types.LocationAddress.LocationAddress}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



