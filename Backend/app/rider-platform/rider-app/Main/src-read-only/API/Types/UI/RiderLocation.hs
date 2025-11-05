{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RiderLocation where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Servant
import Tools.Auth

data BusLocation = BusLocation
  { busNumber :: Data.Text.Text,
    customerLocation :: Kernel.External.Maps.Types.LatLong,
    customerLocationTimestamp :: Kernel.Prelude.UTCTime,
    distanceToBus :: Kernel.Prelude.Double,
    id :: Kernel.Prelude.Maybe Data.Text.Text,
    locationAccuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationRequest = RiderLocationRequest {city :: Kernel.Types.Beckn.Context.City, locationAccuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double, riderLat :: Kernel.Prelude.Double, riderLon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationResponse = RiderLocationResponse {buses :: [BusLocation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
