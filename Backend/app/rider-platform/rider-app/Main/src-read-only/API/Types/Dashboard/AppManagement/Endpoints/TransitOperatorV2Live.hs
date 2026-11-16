{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperatorV2Live where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data LiveStopEta = LiveStopEta
  { delaySeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    predictedArrival :: Kernel.Prelude.UTCTime,
    scheduledArrival :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stopId :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopSeq :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveTrip = LiveTrip
  { actualEnd :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    actualStart :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    routeId :: Kernel.Prelude.Text,
    routeNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scheduledEnd :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    scheduledStart :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripNumber :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveTripStop = LiveTripStop
  { delaySeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    passed :: Kernel.Prelude.Bool,
    predictedArrival :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    scheduledArrival :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopId :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopSeq :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveVehicle = LiveVehicle
  { ageSeconds :: Kernel.Prelude.Int,
    bearing :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    depotId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    depotName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    deviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isUpcoming :: Kernel.Prelude.Bool,
    lastPingAt :: Kernel.Prelude.UTCTime,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    routeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    speedKmph :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveVehicleDetail = LiveVehicleDetail
  { completedTrips :: [LiveTrip],
    conductorToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currentTrip :: Kernel.Prelude.Maybe LiveTrip,
    driverToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nextStops :: [LiveStopEta],
    routePolyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scheduleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripStops :: [LiveTripStop],
    upcomingTrips :: [LiveTrip],
    vehicle :: Kernel.Prelude.Maybe LiveVehicle,
    waybillNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveVehicleList = LiveVehicleList {asOf :: Kernel.Prelude.UTCTime, items :: [LiveVehicle]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("transitOperatorV2" :> (TransitOperatorV2ListLiveVehiclesHelper :<|> TransitOperatorV2GetLiveVehicleHelper))

type TransitOperatorV2ListLiveVehicles =
  ( "live" :> "vehicles" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "routeId" Kernel.Prelude.Text
      :> QueryParam
           "lat"
           Kernel.Prelude.Double
      :> QueryParam "lon" Kernel.Prelude.Double
      :> QueryParam
           "radiusMeters"
           Kernel.Prelude.Double
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           LiveVehicleList
  )

type TransitOperatorV2ListLiveVehiclesHelper =
  ( "live" :> "vehicles" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "routeId" Kernel.Prelude.Text
      :> QueryParam
           "lat"
           Kernel.Prelude.Double
      :> QueryParam "lon" Kernel.Prelude.Double
      :> QueryParam
           "radiusMeters"
           Kernel.Prelude.Double
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           LiveVehicleList
  )

type TransitOperatorV2GetLiveVehicle =
  ( "live" :> "vehicles" :> Capture "vehicleNo" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           LiveVehicleDetail
  )

type TransitOperatorV2GetLiveVehicleHelper =
  ( "live" :> "vehicles" :> Capture "vehicleNo" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) LiveVehicleDetail
  )

data TransitOperatorV2LiveAPIs = TransitOperatorV2LiveAPIs
  { transitOperatorV2ListLiveVehicles :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient LiveVehicleList),
    transitOperatorV2GetLiveVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient LiveVehicleDetail)
  }

mkTransitOperatorV2LiveAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorV2LiveAPIs)
mkTransitOperatorV2LiveAPIs transitOperatorV2LiveClient = (TransitOperatorV2LiveAPIs {..})
  where
    transitOperatorV2ListLiveVehicles :<|> transitOperatorV2GetLiveVehicle = transitOperatorV2LiveClient

data TransitOperatorV2LiveUserActionType
  = TRANSIT_OPERATOR_V2_LIST_LIVE_VEHICLES
  | TRANSIT_OPERATOR_V2_GET_LIVE_VEHICLE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorV2LiveUserActionType)])
