{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.WMB where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.ApprovalRequest
import qualified Domain.Types.Common
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

data ApprovalRequestResp = ApprovalRequestResp {status :: Domain.Types.ApprovalRequest.RequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableRoute = AvailableRoute {destination :: StopInfo, roundRouteCode :: Kernel.Prelude.Maybe Data.Text.Text, routeInfo :: RouteInfo, source :: StopInfo, vehicleDetails :: VehicleDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableRouteReq = AvailableRouteReq {vehicleNumber :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverReqResp = DriverReqResp {requestId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndTripStatus
  = SUCCESS
  | WAITING_FOR_ADMIN_APPROVAL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RequestDetails = RequestDetails {body :: Data.Text.Text, requestData :: Domain.Types.ApprovalRequest.ApprovalRequestData, title :: Data.Text.Text}
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

data TripQrStartReq = TripQrStartReq {location :: Kernel.External.Maps.Types.LatLong, routeCode :: Data.Text.Text, vehicleNumberHash :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripStartReq = TripStartReq {location :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionDetails = TripTransactionDetails
  { destination :: StopInfo,
    endRideApprovalRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest),
    routeInfo :: RouteInfo,
    source :: StopInfo,
    status :: Domain.Types.TripTransaction.TripStatus,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    vehicleNumber :: Data.Text.Text,
    vehicleType :: Domain.Types.Common.ServiceTierType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleDetails = VehicleDetails {_type :: Domain.Types.Common.ServiceTierType, number :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
