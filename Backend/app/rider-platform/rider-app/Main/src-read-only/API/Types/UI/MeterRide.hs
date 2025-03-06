{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MeterRide where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.LocationAddress
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import Servant
import Tools.Auth

data MeterRideAddDestinationReq = MeterRideAddDestinationReq {destinationLatLong :: Kernel.External.Maps.Types.LatLong, destinationLocation :: Domain.Types.LocationAddress.LocationAddress}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
