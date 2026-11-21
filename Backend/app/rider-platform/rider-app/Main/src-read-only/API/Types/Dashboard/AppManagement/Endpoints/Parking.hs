{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Parking where

import qualified "this" API.Types.UI.TicketService
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Data.Time
import qualified "this" Domain.Types.BusinessHour
import qualified "this" Domain.Types.ServiceCategory
import qualified "this" Domain.Types.ServicePeopleCategory
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ParkingClusterResp = ParkingClusterResp
  { geomGeoJson :: Kernel.Prelude.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    isActive :: Kernel.Prelude.Bool,
    name :: Data.Text.Text,
    zoneIds :: [Kernel.Types.Id.Id Domain.Types.TicketService.TicketService]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingClusterStatusReq = ParkingClusterStatusReq {isActive :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ParkingClusterStatusReq where
  hideSecrets = Kernel.Prelude.identity

data ParkingOccupancyResp = ParkingOccupancyResp {name :: Data.Text.Text, serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService, stays :: [ParkingOperatorStay], vehicleTypes :: [ParkingVehicleTypeOccupancy]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingOperatorStay = ParkingOperatorStay
  { amountPaid :: Kernel.Types.Common.HighPrecMoney,
    bookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    categoryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory),
    endTime :: Kernel.Prelude.UTCTime,
    paymentMethod :: Kernel.Prelude.Maybe Domain.Types.TicketBooking.PaymentMethod,
    startTime :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Data.Text.Text,
    verificationCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingVehicleTypeOccupancy = ParkingVehicleTypeOccupancy
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    heldUnpaid :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    occupiedNow :: Kernel.Prelude.Int,
    peopleCategoryId :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingZoneRevenue = ParkingZoneRevenue
  { cashAmount :: Kernel.Types.Common.HighPrecMoney,
    name :: Data.Text.Text,
    onlineAmount :: Kernel.Types.Common.HighPrecMoney,
    paidBookings :: Kernel.Prelude.Int,
    paidExtensions :: Kernel.Prelude.Int,
    placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    totalAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("parking" :> (ListParkingClusters :<|> GetParkingCityRevenue :<|> GetParkingCluster :<|> SetParkingClusterStatus :<|> GetParkingOccupancy :<|> GetParkingClusterRevenue :<|> VerifyParkingTicket :<|> CashBookParking))

type ListParkingClusters = ("clusters" :> Get '[JSON] [ParkingClusterResp])

type GetParkingCityRevenue = ("revenue" :> MandatoryQueryParam "from" Data.Time.Day :> MandatoryQueryParam "to" Data.Time.Day :> Get '[JSON] [ParkingZoneRevenue])

type GetParkingCluster = ("clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> Get '[JSON] ParkingClusterResp)

type SetParkingClusterStatus =
  ( "clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "status" :> ReqBody '[JSON] ParkingClusterStatusReq
      :> Post
           '[JSON]
           ParkingClusterResp
  )

type GetParkingOccupancy =
  ( "clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "zones"
      :> Capture
           "zoneId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> "occupancy"
      :> Get '[JSON] ParkingOccupancyResp
  )

type GetParkingClusterRevenue =
  ( "clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "revenue"
      :> MandatoryQueryParam
           "from"
           Data.Time.Day
      :> MandatoryQueryParam "to" Data.Time.Day
      :> Get '[JSON] [ParkingZoneRevenue]
  )

type VerifyParkingTicket =
  ( "clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "zones"
      :> Capture
           "zoneId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> "verify"
      :> Capture
           "ticketServiceShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> ReqBody
           '[JSON]
           API.Types.UI.TicketService.TicketServiceVerificationReq
      :> Post
           '[JSON]
           API.Types.UI.TicketService.TicketServiceVerificationResp
  )

type CashBookParking =
  ( "clusters" :> Capture "clusterId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "zones"
      :> Capture
           "zoneId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> "cashBook"
      :> ReqBody
           '[JSON]
           API.Types.UI.TicketService.DirectTicketBookingReq
      :> Post
           '[JSON]
           API.Types.UI.TicketService.DirectTicketBookingResp
  )

data ParkingAPIs = ParkingAPIs
  { listParkingClusters :: EulerHS.Types.EulerClient [ParkingClusterResp],
    getParkingCityRevenue :: Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient [ParkingZoneRevenue],
    getParkingCluster :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> EulerHS.Types.EulerClient ParkingClusterResp,
    setParkingClusterStatus :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> ParkingClusterStatusReq -> EulerHS.Types.EulerClient ParkingClusterResp,
    getParkingOccupancy :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> EulerHS.Types.EulerClient ParkingOccupancyResp,
    getParkingClusterRevenue :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient [ParkingZoneRevenue],
    verifyParkingTicket :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> API.Types.UI.TicketService.TicketServiceVerificationReq -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketServiceVerificationResp,
    cashBookParking :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> API.Types.UI.TicketService.DirectTicketBookingReq -> EulerHS.Types.EulerClient API.Types.UI.TicketService.DirectTicketBookingResp
  }

mkParkingAPIs :: (Client EulerHS.Types.EulerClient API -> ParkingAPIs)
mkParkingAPIs parkingClient = (ParkingAPIs {..})
  where
    listParkingClusters :<|> getParkingCityRevenue :<|> getParkingCluster :<|> setParkingClusterStatus :<|> getParkingOccupancy :<|> getParkingClusterRevenue :<|> verifyParkingTicket :<|> cashBookParking = parkingClient

data ParkingUserActionType
  = LIST_PARKING_CLUSTERS
  | GET_PARKING_CITY_REVENUE
  | GET_PARKING_CLUSTER
  | SET_PARKING_CLUSTER_STATUS
  | GET_PARKING_OCCUPANCY
  | GET_PARKING_CLUSTER_REVENUE
  | VERIFY_PARKING_TICKET
  | CASH_BOOK_PARKING
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''ParkingUserActionType])
