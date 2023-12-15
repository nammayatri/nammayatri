{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketService where

import API.Types.UI.TicketService (BusinessHourResp, CategoriesResp, PeopleCategoriesResp, PeopleCategoriesVerificationRes, TicketBookingAPIEntity, TicketBookingCategoryDetails, TicketBookingCategoryReq, TicketBookingDetails, TicketBookingPeopleCategoryDetails, TicketBookingPeopleCategoryReq, TicketBookingReq, TicketBookingServiceDetails, TicketBookingServicesReq, TicketBookingUpdateSeatsReq, TicketServiceResp, TicketServiceVerificationResp, TicketVerificationStatus)
import qualified API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.UI.TicketService as Domain.Action.UI.TicketService
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TicketService
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "ticket" :> "places" :> Get '[JSON] [Domain.Types.TicketPlace.TicketPlace]
    :<|> TokenAuth :> "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "services" :> QueryParam "date" (Data.Time.Calendar.Day) :> Get '[JSON] [API.Types.UI.TicketService.TicketServiceResp]
    :<|> TokenAuth :> "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "book" :> ReqBody '[JSON] API.Types.UI.TicketService.TicketBookingReq :> Post '[JSON] Kernel.External.Payment.Interface.Types.CreateOrderResp
    :<|> TokenAuth :> "ticket" :> "bookings" :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> MandatoryQueryParam "status" (Domain.Types.TicketBooking.BookingStatus) :> Get '[JSON] [API.Types.UI.TicketService.TicketBookingAPIEntity]
    :<|> TokenAuth :> "ticket" :> "bookings" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "details" :> Get '[JSON] API.Types.UI.TicketService.TicketBookingDetails
    :<|> TokenAuth :> "ticket" :> "bookings" :> Capture "personServiceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService) :> Capture "ticketServiceShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService) :> "verify" :> Post '[JSON] API.Types.UI.TicketService.TicketServiceVerificationResp
    :<|> TokenAuth :> "ticket" :> "bookings" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "status" :> Get '[JSON] Domain.Types.TicketBooking.BookingStatus
    :<|> TokenAuth :> "ticket" :> "bookings" :> "update" :> "seats" :> ReqBody '[JSON] API.Types.UI.TicketService.TicketBookingUpdateSeatsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler =
  getTicketPlaces
    :<|> getTicketPlacesServices
    :<|> postTicketPlacesBook
    :<|> getTicketBookings
    :<|> getTicketBookingsDetails
    :<|> postTicketBookingsVerify
    :<|> getTicketBookingsStatus
    :<|> postTicketBookingsUpdateSeats

getTicketPlaces :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlaces (Kernel.Prelude.first Kernel.Prelude.Just a1)

getTicketPlacesServices :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp]
getTicketPlacesServices a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlacesServices (Kernel.Prelude.first Kernel.Prelude.Just a3) a2 a1

postTicketPlacesBook :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp
postTicketPlacesBook a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketPlacesBook (Kernel.Prelude.first Kernel.Prelude.Just a3) a2 a1

getTicketBookings :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.FlowHandler [API.Types.UI.TicketService.TicketBookingAPIEntity]
getTicketBookings a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookings (Kernel.Prelude.first Kernel.Prelude.Just a4) a3 a2 a1

getTicketBookingsDetails :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails
getTicketBookingsDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookingsDetails (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postTicketBookingsVerify :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp
postTicketBookingsVerify a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsVerify (Kernel.Prelude.first Kernel.Prelude.Just a3) a2 a1

getTicketBookingsStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Domain.Types.TicketBooking.BookingStatus
getTicketBookingsStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookingsStatus (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postTicketBookingsUpdateSeats :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postTicketBookingsUpdateSeats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsUpdateSeats (Kernel.Prelude.first Kernel.Prelude.Just a2) a1
