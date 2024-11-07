module Domain.Action.Dashboard.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
  )
where

import qualified "this" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.UI.TicketService
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postTicketsVerify ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketService.TicketService ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService ->
  Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp
postTicketsVerify merchantShortId _opCity personServiceId ticketBookingServiceShortId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingsVerify (Nothing, m.id) personServiceId ticketBookingServiceShortId

postTicketsServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
postTicketsServices merchantShortId _opCity ticketPlaceId date = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlacesServices (Nothing, m.id) ticketPlaceId date

getTicketsPlaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketsPlaces merchantShortId _opCity = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlaces (Nothing, m.id)

postTicketsUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.TicketService.TicketBookingUpdateSeatsReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsUpdate merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingsUpdateSeats (Nothing, m.id) req

postTicketsBookingsCancel ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.TicketService.TicketBookingCancelReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsBookingsCancel merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingCancel (Nothing, m.id) req

postTicketsServiceCancel ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.TicketService.TicketServiceCancelReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsServiceCancel merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketServiceCancel (Nothing, m.id) req

getTicketsBookingDetails ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
  Environment.Flow API.Types.UI.TicketService.TicketBookingDetails
getTicketsBookingDetails merchantShortId _opCity ticketBookingShortId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketBookingsDetails (Nothing, m.id) ticketBookingShortId
