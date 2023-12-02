{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.UI.TicketService where

import Domain.Action.UI.TicketService (BusinessHourResp, CategoriesResp, PeopleCategoriesResp, TickectBookingServiceDetails, TicketBookingAPIEntity, TicketBookingCategoryDetails, TicketBookingCategoryReq, TicketBookingDetails, TicketBookingPeopleCategoryDetails, TicketBookingPeopleCategoryReq, TicketBookingReq, TicketBookingServicesReq, TicketBookingUpdateSeatsReq, TicketServiceResp)
import qualified Domain.Action.UI.TicketService as Domain.Action.UI.TicketService
import qualified Domain.Action.UI.Tickets as Domain.Action.UI.Tickets
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.TicketBooking as Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService as Domain.Types.TicketBookingService
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import qualified Environment as Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types as Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.APISuccess as Kernel.Types.APISuccess
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  TokenAuth :> "ticket" :> "places" :> Get '[JSON] [Domain.Types.TicketPlace.TicketPlace]
    :<|> TokenAuth :> "v2" :> "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "services" :> Get '[JSON] [Domain.Action.UI.TicketService.TicketServiceResp]
    :<|> TokenAuth :> "v2" :> "ticket" :> "places" :> Capture "placeId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "book" :> ReqBody '[JSON] Domain.Action.UI.TicketService.TicketBookingReq :> Post '[JSON] Kernel.External.Payment.Interface.Types.CreateOrderResp
    :<|> TokenAuth :> "ticket" :> "places" :> "bookings" :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> MandatoryQueryParam "status" (Domain.Types.TicketBooking.BookingStatus) :> Get '[JSON] [Domain.Action.UI.TicketService.TicketBookingAPIEntity]
    :<|> TokenAuth :> "v2" :> "ticket" :> "places" :> "bookings" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "details" :> Get '[JSON] Domain.Action.UI.TicketService.TicketBookingDetails
    :<|> TokenAuth :> "ticket" :> "places" :> "bookings" :> Capture "personServiceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService) :> Capture "ticketServiceShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService) :> "verify" :> Post '[JSON] Domain.Action.UI.Tickets.TicketServiceVerificationResp
    :<|> TokenAuth :> "ticket" :> "places" :> "bookings" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "status" :> Get '[JSON] Domain.Types.TicketBooking.BookingStatus
    :<|> TokenAuth :> "ticket" :> "places" :> "bookings" :> "update" :> "seats" :> ReqBody '[JSON] Domain.Action.UI.TicketService.TicketBookingUpdateSeatsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler =
  getTicketPlaces
    :<|> getV2TicketPlacesServices
    :<|> postV2TicketPlacesBook
    :<|> getTicketPlacesBookings
    :<|> getV2TicketPlacesBookingsDetails
    :<|> postTicketPlacesBookingsVerify
    :<|> getTicketPlacesBookingsStatus
    :<|> postTicketPlacesBookingsUpdateSeats

getTicketPlaces :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlaces a1

getV2TicketPlacesServices :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler [Domain.Action.UI.TicketService.TicketServiceResp]
getV2TicketPlacesServices a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getV2TicketPlacesServices a2 a1

postV2TicketPlacesBook :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Action.UI.TicketService.TicketBookingReq -> Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp
postV2TicketPlacesBook a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postV2TicketPlacesBook a3 a2 a1

getTicketPlacesBookings :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.FlowHandler [Domain.Action.UI.TicketService.TicketBookingAPIEntity]
getTicketPlacesBookings a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlacesBookings a4 a3 a2 a1

getV2TicketPlacesBookingsDetails :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Domain.Action.UI.TicketService.TicketBookingDetails
getV2TicketPlacesBookingsDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getV2TicketPlacesBookingsDetails a2 a1

postTicketPlacesBookingsVerify :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.FlowHandler Domain.Action.UI.Tickets.TicketServiceVerificationResp
postTicketPlacesBookingsVerify a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketPlacesBookingsVerify a3 a2 a1

getTicketPlacesBookingsStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Domain.Types.TicketBooking.BookingStatus
getTicketPlacesBookingsStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlacesBookingsStatus a2 a1

postTicketPlacesBookingsUpdateSeats :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Action.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postTicketPlacesBookingsUpdateSeats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketPlacesBookingsUpdateSeats a2 a1
