{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ParkingBooking where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TicketService
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

data MarshalPersonReq = MarshalPersonReq
  { email :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Kernel.Prelude.Maybe Data.Text.Text,
    lastName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantShortId :: Data.Text.Text,
    mobileCountryCode :: Data.Text.Text,
    mobileNumber :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MarshalPersonResp = MarshalPersonResp {customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingBookingReq = ParkingBookingReq
  { amount :: Kernel.Types.Common.HighPrecMoney,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    entityId :: Data.Text.Text,
    parkingEndTime :: Kernel.Prelude.UTCTime,
    parkingStartTime :: Kernel.Prelude.UTCTime,
    slotNumber :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingBookingResponse = ParkingBookingResponse
  { orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    orderShortId :: Data.Text.Text,
    parkingLotId :: Data.Text.Text,
    paymentLink :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingExtendReq = ParkingExtendReq {durationMinutes :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingStayResp = ParkingStayResp
  { amount :: Kernel.Types.Common.HighPrecMoney,
    bookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    categoryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory),
    endTime :: Kernel.Prelude.UTCTime,
    placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    placeName :: Data.Text.Text,
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    startTime :: Kernel.Prelude.UTCTime,
    status :: ParkingStayStatus,
    ticketBookingShortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking,
    vehicleNumber :: Data.Text.Text,
    vehicleType :: Kernel.Prelude.Maybe Data.Text.Text,
    zoneName :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingStayStatus
  = Reserved
  | PendingPayment
  | Active
  | Ended
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingStopResp = ParkingStopResp {durationMinutes :: Kernel.Prelude.Int, finalAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingVehicleTypeResp = ParkingVehicleTypeResp
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    durationTariff :: Kernel.Prelude.Maybe Data.Aeson.Value,
    freeSpaces :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Data.Text.Text,
    peopleCategoryId :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingZoneResp = ParkingZoneResp
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    clusterName :: Data.Text.Text,
    distanceMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    geomGeoJson :: Kernel.Prelude.Maybe Data.Text.Text,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    maxDurationMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Data.Text.Text,
    placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    vehicleTypes :: [ParkingVehicleTypeResp]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
