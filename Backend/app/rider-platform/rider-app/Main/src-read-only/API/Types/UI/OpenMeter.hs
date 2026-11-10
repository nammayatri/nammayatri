{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.OpenMeter where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.OpenMeterRide
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data OpenMeterRideEndReq = OpenMeterRideEndReq
  { currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    distanceMeters :: Kernel.Types.Common.Meters,
    encodedPolyline :: Kernel.Prelude.Text,
    endLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endReason :: Domain.Types.OpenMeterRide.OpenMeterEndReason,
    endTime :: Kernel.Prelude.UTCTime,
    estimatedGapMeters :: Kernel.Types.Common.Meters,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pointsCount :: Kernel.Prelude.Int,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenMeterRideHeartbeatReq = OpenMeterRideHeartbeatReq
  { distanceMeters :: Kernel.Types.Common.Meters,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    lastLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    sampledAt :: Kernel.Prelude.UTCTime,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenMeterRideRes = OpenMeterRideRes
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    distanceMeters :: Kernel.Types.Common.Meters,
    encodedPolyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedGapMeters :: Kernel.Types.Common.Meters,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide,
    pointsCount :: Kernel.Prelude.Int,
    serverLinearDistanceMeters :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.OpenMeterRide.OpenMeterRideStatus,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenMeterRideStartReq = OpenMeterRideStartReq
  { rideId :: Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide,
    startLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startTime :: Kernel.Prelude.UTCTime,
    tariffSnapshot :: Kernel.Prelude.Maybe Data.Aeson.Value,
    trackingConfig :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
