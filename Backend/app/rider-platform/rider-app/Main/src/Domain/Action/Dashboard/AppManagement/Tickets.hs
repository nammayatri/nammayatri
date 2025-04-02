module Domain.Action.Dashboard.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
    postTicketsTicketdashboardRegister,
    postTicketsTicketdashboardLoginAuth,
    postTicketsTicketdashboardLoginVerify,
    getTicketsTicketdashboardAgreement,
  )
where

import qualified API.Types.Dashboard.AppManagement.Tickets
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

postTicketsTicketdashboardRegister ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq ->
  Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
postTicketsTicketdashboardRegister merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketDashboardRegister m req

postTicketsTicketdashboardLoginAuth ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsTicketdashboardLoginAuth merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketDashboardLoginAuth m req

postTicketsTicketdashboardLoginVerify ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq ->
  Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp
postTicketsTicketdashboardLoginVerify merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketDashboardLoginVerify m req

getTicketsTicketdashboardAgreement ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp
getTicketsTicketdashboardAgreement merchantShortId _opCity templateName = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketDashboardAgreement m templateName
