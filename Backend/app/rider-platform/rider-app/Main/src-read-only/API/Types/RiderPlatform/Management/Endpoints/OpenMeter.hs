{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.OpenMeter where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.OpenMeterRide
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data OpenMeterRideInfoRes = OpenMeterRideInfoRes
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    customerId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    distanceMeters :: Kernel.Types.Common.Meters,
    encodedPolyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endReason :: Kernel.Prelude.Maybe Domain.Types.OpenMeterRide.OpenMeterEndReason,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedGapMeters :: Kernel.Types.Common.Meters,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    lastHeartbeatAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    pointsCount :: Kernel.Prelude.Int,
    rideId :: Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide,
    serverLinearDistanceMeters :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    serverStartedAt :: Kernel.Prelude.UTCTime,
    startLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.OpenMeterRide.OpenMeterRideStatus,
    tariffSnapshot :: Kernel.Prelude.Maybe Data.Aeson.Value,
    trackingConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenMeterRideListItem = OpenMeterRideListItem
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    customerId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    distanceMeters :: Kernel.Types.Common.Meters,
    endReason :: Kernel.Prelude.Maybe Domain.Types.OpenMeterRide.OpenMeterEndReason,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideId :: Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide,
    serverLinearDistanceMeters :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.OpenMeterRide.OpenMeterRideStatus,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenMeterRideListRes = OpenMeterRideListRes {rides :: [OpenMeterRideListItem], summary :: Dashboard.Common.Summary, totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("openMeter" :> (GetOpenMeterRideList :<|> GetOpenMeterRideInfo))

type GetOpenMeterRideList =
  ( "ride" :> "list" :> QueryParam "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.OpenMeterRide.OpenMeterRideStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           OpenMeterRideListRes
  )

type GetOpenMeterRideInfo = ("ride" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide) :> "info" :> Get ('[JSON]) OpenMeterRideInfoRes)

data OpenMeterAPIs = OpenMeterAPIs
  { getOpenMeterRideList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.OpenMeterRide.OpenMeterRideStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient OpenMeterRideListRes),
    getOpenMeterRideInfo :: (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> EulerHS.Types.EulerClient OpenMeterRideInfoRes)
  }

mkOpenMeterAPIs :: (Client EulerHS.Types.EulerClient API -> OpenMeterAPIs)
mkOpenMeterAPIs openMeterClient = (OpenMeterAPIs {..})
  where
    getOpenMeterRideList :<|> getOpenMeterRideInfo = openMeterClient

data OpenMeterUserActionType
  = GET_OPEN_METER_RIDE_LIST
  | GET_OPEN_METER_RIDE_INFO
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''OpenMeterUserActionType)])
