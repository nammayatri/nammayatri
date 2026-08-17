{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSFleetOperator where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FleetOperatorTripAction
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound
import Servant
import Tools.Auth

data BusTripScheduleResp = BusTripScheduleResp {schedules :: [FleetBusTripSchedule]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI
  { code :: Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    integratedBppConfigId :: Data.Text.Text,
    longName :: Data.Text.Text,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stops :: Kernel.Prelude.Maybe [FRFSStationAPI],
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    totalStops :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    waypoints :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Kernel.Prelude.Maybe Data.Text.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    integratedBppConfigId :: Data.Text.Text,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Kernel.Prelude.Maybe Data.Text.Text,
    parentStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    routeCodes :: Kernel.Prelude.Maybe [Data.Text.Text],
    sequenceNum :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stationType :: Kernel.Prelude.Maybe Data.Text.Text,
    timeTakenToTravelUpcomingStop :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    towards :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTripPassengerManifestResp = FRFSTripPassengerManifestResp {manifest :: [PassengerStopManifest]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBusStopETA = FleetBusStopETA
  { arrivalTime :: Kernel.Prelude.UTCTime,
    arrivalTimeUnix :: Kernel.Prelude.Int,
    etaSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopCode :: Data.Text.Text,
    stopName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBusTripSchedule = FleetBusTripSchedule
  { eta :: [FleetBusStopETA],
    isActiveTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTier :: BecknV2.FRFS.Enums.ServiceTierType,
    tripNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleNo :: Data.Text.Text,
    waybillNo :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorCurrentOperationReq = FleetOperatorCurrentOperationReq {gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text, gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text, vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorCurrentOperationResp = FleetOperatorCurrentOperationResp
  { current :: Kernel.Prelude.Maybe OperatorTripInfo,
    gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text,
    gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    gtfsId :: Data.Text.Text,
    history :: [OperatorTripInfo],
    upcoming :: [OperatorTripInfo],
    vehicleNumber :: Data.Text.Text,
    waybillNo :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorTripActionReq = FleetOperatorTripActionReq
  { action :: Domain.Types.FleetOperatorTripAction.FleetOperatorTripAction,
    gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text,
    gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorTripActionResp = FleetOperatorTripActionResp {currentTripNumber :: Kernel.Prelude.Int, hasUpcomingTrips :: Kernel.Prelude.Bool}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorTripInfo = OperatorTripInfo
  { dutyDate :: Kernel.Prelude.Maybe Data.Text.Text,
    endTime :: Kernel.Prelude.Maybe Data.Text.Text,
    isActiveTrip :: Kernel.Prelude.Bool,
    routeId :: Data.Text.Text,
    routeName :: Data.Text.Text,
    routeNumber :: Data.Text.Text,
    startTime :: Kernel.Prelude.Maybe Data.Text.Text,
    tripNumber :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassengerInfo = PassengerInfo {bookingId :: Data.Text.Text, checkedIn :: Kernel.Prelude.Bool, name :: Data.Text.Text, personId :: Data.Text.Text, phone :: Data.Text.Text, quantity :: Kernel.Prelude.Int}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassengerStopManifest = PassengerStopManifest {alightingPassengers :: [PassengerInfo], boardingPassengers :: [PassengerInfo], stopCode :: Data.Text.Text, stopName :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
