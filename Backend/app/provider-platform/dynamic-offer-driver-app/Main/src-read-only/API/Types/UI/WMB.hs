{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.WMB where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.Common
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.TripTransaction
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data ActiveTripTransaction = ActiveTripTransaction {tripTransactionDetails :: Kernel.Prelude.Maybe TripTransactionDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlertReqResp = AlertReqResp {requestId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlertRequestResp = AlertRequestResp {status :: Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableBadge = AvailableBadge {badgeName :: Data.Text.Text, badgeType :: Domain.Types.FleetBadgeType.FleetBadgeType, isActive :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableRoute = AvailableRoute {destination :: StopInfo, roundRouteCode :: Kernel.Prelude.Maybe Data.Text.Text, routeInfo :: RouteInfo, source :: StopInfo, vehicleDetails :: VehicleDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableRouteReq = AvailableRouteReq {vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndTripStatus
  = SUCCESS
  | WAITING_FOR_ADMIN_APPROVAL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RequestDetails = RequestDetails {body :: Data.Text.Text, requestData :: Domain.Types.Alert.AlertRequestData.AlertRequestData, title :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteInfo = RouteInfo {code :: Data.Text.Text, endPoint :: Kernel.External.Maps.Types.LatLong, longName :: Data.Text.Text, shortName :: Data.Text.Text, startPoint :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StopInfo = StopInfo {code :: Data.Text.Text, name :: Data.Text.Text, point :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripEndReq = TripEndReq {location :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripEndResp = TripEndResp {requestId :: Kernel.Prelude.Maybe Data.Text.Text, result :: EndTripStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripQrStartReq = TripQrStartReq
  { conductorBadgeName :: Kernel.Prelude.Maybe Data.Text.Text,
    driverBadgeName :: Kernel.Prelude.Maybe Data.Text.Text,
    location :: Kernel.External.Maps.Types.LatLong,
    routeCode :: Data.Text.Text,
    vehicleNumberHash :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripStartReq = TripStartReq {location :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionDetails = TripTransactionDetails
  { conductorName :: Kernel.Prelude.Maybe Data.Text.Text,
    destination :: StopInfo,
    driverName :: Kernel.Prelude.Maybe Data.Text.Text,
    dutyType :: Kernel.Prelude.Maybe Data.Text.Text,
    endRideApprovalRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest),
    routeInfo :: RouteInfo,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    source :: StopInfo,
    status :: Domain.Types.TripTransaction.TripStatus,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    tripType :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.TripType,
    vehicleNumber :: Data.Text.Text,
    vehicleType :: Domain.Types.Common.ServiceTierType,
    vipName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleDetails = VehicleDetails {_type :: Domain.Types.Common.ServiceTierType, number :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
