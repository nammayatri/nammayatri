{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.TicketService where

import Data.OpenApi (ToSchema)
import qualified Data.Text as Data.Text
import Data.Time (dayOfWeek, utctDay)
import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Action.UI.Tickets as Domain.Action.UI.Tickets
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory as Domain.Types.ServicePeopleCategory
import qualified Domain.Types.SpecialOccasion as Domain.Types.SpecialOccasion
import qualified Domain.Types.TicketBooking as DTTB
import qualified Domain.Types.TicketBooking as Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService as DTB
import qualified Domain.Types.TicketBookingService as Domain.Types.TicketBookingService
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import qualified Environment as Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types as Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.APISuccess as Kernel.Types.APISuccess
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant
import qualified Storage.Queries.BusinessHour as QBH
import qualified Storage.Queries.SeatManagement as QTSM
import qualified Storage.Queries.ServiceCategory as QSC
import qualified Storage.Queries.ServicePeopleCategory as QPC
import qualified Storage.Queries.SpecialOccasion as QSO
import qualified Storage.Queries.TicketBooking as QTB
import qualified Storage.Queries.TicketBookingService as QTBS
import qualified Storage.Queries.TicketService as QTS
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

data BusinessHourResp = BusinessHourResp
  { categories :: [Domain.Action.UI.TicketService.CategoriesResp],
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    id :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    operationalDays :: [Data.Text.Text],
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    specialDayDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    specialDayType :: Kernel.Prelude.Maybe Domain.Types.SpecialOccasion.SpecialDayType,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CategoriesResp = CategoriesResp
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookedSeats :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    name :: Data.Text.Text,
    peopleCategories :: [Domain.Action.UI.TicketService.PeopleCategoriesResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PeopleCategoriesResp = PeopleCategoriesResp
  { description :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Data.Text.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TickectBookingServiceDetails = TickectBookingServiceDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    categories :: [Domain.Action.UI.TicketService.TicketBookingCategoryDetails],
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceShortId :: Data.Text.Text,
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    ticketServiceName :: Data.Text.Text,
    verificationCount :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingAPIEntity = TicketBookingAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    personId :: Data.Text.Text,
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryDetails = TicketBookingCategoryDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    peopleCategories :: [Domain.Action.UI.TicketService.TicketBookingPeopleCategoryDetails]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryReq = TicketBookingCategoryReq
  { categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    peopleCategories :: [Domain.Action.UI.TicketService.TicketBookingPeopleCategoryReq]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingDetails = TicketBookingDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    personId :: Data.Text.Text,
    services :: [Domain.Action.UI.TicketService.TickectBookingServiceDetails],
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryDetails = TicketBookingPeopleCategoryDetails
  { name :: Data.Text.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryReq = TicketBookingPeopleCategoryReq
  { numberOfUnits :: Kernel.Prelude.Int,
    peopleCategoryId :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingReq = TicketBookingReq
  { services :: [Domain.Action.UI.TicketService.TicketBookingServicesReq],
    visitDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingServicesReq = TicketBookingServicesReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    categories :: [Domain.Action.UI.TicketService.TicketBookingCategoryReq],
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingUpdateSeatsReq = TicketBookingUpdateSeatsReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    date :: Data.Time.Calendar.Day,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    updatedBookedSeats :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketServiceResp = TicketServiceResp
  { allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [Domain.Action.UI.TicketService.BusinessHourResp],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    maxVerification :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    placesId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getTicketPlaces :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces = error "Logic yet to be decided"

getV2TicketPlacesServices :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow [Domain.Action.UI.TicketService.TicketServiceResp]
getV2TicketPlacesServices = error "Logic yet to be decided"

postV2TicketPlacesBook :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Action.UI.TicketService.TicketBookingReq -> Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp
postV2TicketPlacesBook = error "Logic yet to be decided"

getTicketPlacesBookings :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.Flow [Domain.Action.UI.TicketService.TicketBookingAPIEntity]
getTicketPlacesBookings = error "Logic yet to be decided"

getV2TicketPlacesBookingsDetails :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Action.UI.TicketService.TicketBookingDetails
getV2TicketPlacesBookingsDetails = error "Logic yet to be decided"

postTicketPlacesBookingsVerify :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow Domain.Action.UI.Tickets.TicketServiceVerificationResp
postTicketPlacesBookingsVerify = error "Logic yet to be decided"

getTicketPlacesBookingsStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Types.TicketBooking.BookingStatus
getTicketPlacesBookingsStatus (personId, merchantId) (Kernel.Types.Id.ShortId shortId) = do
  let commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  order <- QOrder.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (PaymentOrderNotFound shortId)
  ticketBooking' <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId)
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then return ticketBooking'.status
    else do
      paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
      case paymentStatus of
        DPayment.PaymentStatus {..} -> do
          when (status == Payment.CHARGED) $ do
            QTB.updateStatusByShortId (Kernel.Types.Id.ShortId shortId) DTTB.Booked
            QTBS.updateAllStatusByBookingId ticketBooking'.id DTB.Confirmed
          when (status `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
            QTB.updateStatusByShortId (Kernel.Types.Id.ShortId shortId) DTTB.Failed
            QTBS.updateAllStatusByBookingId ticketBooking'.id DTB.Failed
        _ -> return ()
      ticketBooking <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId) -- fetch again for updated status
      return ticketBooking.status

postTicketPlacesBookingsUpdateSeats :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Action.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketPlacesBookingsUpdateSeats (_, _) TicketBookingUpdateSeatsReq {..} = do
  void $ QTSM.updateBookedSeats updatedBookedSeats categoryId date
  pure Kernel.Types.APISuccess.Success
