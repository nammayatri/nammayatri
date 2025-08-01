{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TicketService where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified Domain.Types.SpecialOccasion
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingPeopleCategory
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TicketService
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Refunds
import Servant
import Tools.Auth

data BusinessHourResp = BusinessHourResp
  { categories :: [CategoriesResp],
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    id :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    operationalDate :: Kernel.Prelude.Maybe Domain.Types.TicketService.OperationalDate,
    operationalDays :: [Data.Text.Text],
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    specialDayDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    specialDayType :: Kernel.Prelude.Maybe Domain.Types.SpecialOccasion.SpecialDayType,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CategoriesResp = CategoriesResp
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookedSeats :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    isClosed :: Kernel.Prelude.Bool,
    name :: Data.Text.Text,
    peopleCategories :: [PeopleCategoriesResp]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PeopleCategoriesResp = PeopleCategoriesResp
  { cancellationCharges :: Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge],
    description :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Data.Text.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney,
    pricePerUnitWithCurrency :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PeopleCategoriesVerificationRes = PeopleCategoriesVerificationRes
  { id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Data.Text.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney,
    pricePerUnitWithCurrency :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingAPIEntity = TicketBookingAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    personId :: Data.Text.Text,
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingCancelReq = TicketBookingCancelReq {ticketBookingServices :: [TicketBookingServiceCancelReq], ticketBookingShortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryDetails = TicketBookingCategoryDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    bookedSeats :: Kernel.Prelude.Int,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    name :: Data.Text.Text,
    peopleCategories :: [TicketBookingPeopleCategoryDetails],
    serviceCategoryId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryReq = TicketBookingCategoryReq {categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory, peopleCategories :: [TicketBookingPeopleCategoryReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingDetails = TicketBookingDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    personId :: Data.Text.Text,
    refundDetails :: [Lib.Payment.Domain.Types.Refunds.Refunds],
    services :: [TicketBookingServiceDetails],
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryDetails = TicketBookingPeopleCategoryDetails
  { amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancelCharges :: Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge],
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory,
    name :: Data.Text.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    numberOfUnitsCancelled :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney,
    pricePerUnitWithCurrency :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryReq = TicketBookingPeopleCategoryReq {numberOfUnits :: Kernel.Prelude.Int, peopleCategoryId :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingReq = TicketBookingReq {services :: [TicketBookingServicesReq], visitDate :: Data.Time.Calendar.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingServiceCancelReq = TicketBookingServiceCancelReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    serviceCategory :: [TicketBookingServiceCategoryCancelReq],
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingServiceCategoryCancelReq = TicketBookingServiceCategoryCancelReq
  { id :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    peopleCategory :: [TicketBookingServicePeopleCategoryCancelReq],
    serviceCategoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingServiceDetails = TicketBookingServiceDetails
  { allowCancellation :: Kernel.Prelude.Bool,
    amount :: Kernel.Types.Common.HighPrecMoney,
    amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    businessHourId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour),
    categories :: [TicketBookingCategoryDetails],
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    ticketServiceName :: Data.Text.Text,
    ticketServiceShortId :: Data.Text.Text,
    verificationCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingServicePeopleCategoryCancelReq = TicketBookingServicePeopleCategoryCancelReq {id :: Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory, quantity :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingServicesReq = TicketBookingServicesReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    categories :: [TicketBookingCategoryReq],
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketBookingUpdateSeatsReq = TicketBookingUpdateSeatsReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    date :: Data.Time.Calendar.Day,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    updatedBookedSeats :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketPlaceAvailability = TicketPlaceAvailability {closedDays :: [Kernel.Prelude.Int], month :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketServiceCancelReq = TicketServiceCancelReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    date :: Data.Time.Calendar.Day,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    ticketServiceCategoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketServiceResp = TicketServiceResp
  { allowCancellation :: Kernel.Prelude.Bool,
    allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [BusinessHourResp],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    maxVerification :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    placesId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    shortDesc :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketServiceVerificationResp = TicketServiceVerificationResp
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    amountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    categories :: [TicketBookingCategoryDetails],
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    message :: Data.Text.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    status :: TicketVerificationStatus,
    ticketServiceName :: Kernel.Prelude.Maybe Data.Text.Text,
    ticketServiceShortId :: Kernel.Prelude.Maybe Data.Text.Text,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketVerificationStatus
  = BookingSuccess
  | BookingExpired
  | BookingFuture
  | BookingAlreadyVerified
  | DifferentService
  | PaymentPending
  | InvalidBooking
  | CancelledBooking
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
