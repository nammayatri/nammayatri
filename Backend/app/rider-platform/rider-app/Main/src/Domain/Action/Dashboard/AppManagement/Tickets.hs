module Domain.Action.Dashboard.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    getTicketFleetVehicles,
    getTicketFleetVehiclesV2,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
    postTicketsTicketdashboardRegister,
    postTicketsTicketdashboardLoginAuth,
    postTicketsTicketdashboardLoginVerify,
    getTicketsTicketdashboardAgreement,
    getTicketsTicketdashboardUserInfo,
    getTicketsTicketdashboardFile,
    postTicketsTicketdashboardSendverifyotp,
    getTicketsTicketdashboardTicketplaceInfo,
    postTicketsTicketdashboardTicketplaceUpdate,
    getTicketsTicketdashboardTicketplaces,
    getTicketsTicketdashboardTicketplaceSubPlaces,
    postTicketsTicketdashboardTicketplaceUpdateSubPlaces,
    postTicketBookingsVerifyV2,
    postTicketPlacesBook,
    getTicketPlaces,
    getTicketPlaceServices,
    getTicketBookingDetails,
    getAllTicketBookings,
    postTicketBookingCashCollect,
    postTicketPlacesDirectBook,
    getTicketsDashboardBookingStatus,
    getTicketPlaceBookings,
  )
where

import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified "this" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.UI.TicketDashboard
import qualified Domain.Action.UI.TicketService
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketDashboard
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified "this" Domain.Types.TicketSubPlace
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postTicketsVerify ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketService.TicketService ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp
postTicketsVerify merchantShortId _opCity personServiceId ticketBookingServiceShortId mbFleetOwnerId mbVehicleId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingsVerify (Nothing, m.id) personServiceId ticketBookingServiceShortId mbFleetOwnerId mbVehicleId

postTicketsServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Kernel.Prelude.Maybe
    (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) ->
  Environment.Flow
    [API.Types.UI.TicketService.TicketServiceResp]
postTicketsServices merchantShortId _opCity ticketPlaceId date ticketSubPlaceId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlacesServices (Nothing, m.id) ticketPlaceId date ticketSubPlaceId

getTicketsPlaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketsPlaces merchantShortId _opCity = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlaces (Nothing, m.id)

getTicketFleetVehicles ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Environment.Flow [API.Types.UI.TicketService.TicketFleetVehicleResp]
getTicketFleetVehicles merchantShortId _opCity placeId mbLimit mbOffset mbSearchString = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketFleetVehicles (Nothing, m.id) placeId mbLimit mbOffset mbSearchString

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

getTicketsTicketdashboardUserInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardUserInfo
getTicketsTicketdashboardUserInfo _merchantShortId _opCity mbuserId mbuserRole _ = do
  userId <- mbuserId & fromMaybeM (InvalidRequest "User ID is required")
  userRole <- mbuserRole & fromMaybeM (InvalidRequest "User Role is required")
  Domain.Action.UI.TicketDashboard.getTicketDashboardUserInfo userId userRole

getTicketsTicketdashboardFile ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse
getTicketsTicketdashboardFile _merchantShortId _opCity fileId _requestorId _requestorRole = do
  Domain.Action.UI.TicketDashboard.getTicketDashboardFile fileId

postTicketsTicketdashboardSendverifyotp ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Tickets.SendVerifyOtpReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsTicketdashboardSendverifyotp merchantShortId _opCity req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketDashboardSendVerifyOtp m req

getTicketsTicketdashboardTicketplaceInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Environment.Flow Domain.Types.TicketDashboard.TicketPlaceDashboardDetails
getTicketsTicketdashboardTicketplaceInfo _merchantShortId _opCity ticketPlaceId _requestorId' _requestorRole = do
  Domain.Action.UI.TicketDashboard.getTicketPlaceDashboardDetails ticketPlaceId _requestorId' _requestorRole

postTicketsTicketdashboardTicketplaceUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Domain.Types.TicketDashboard.TicketPlaceDashboardDetails ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsTicketdashboardTicketplaceUpdate _merchantShortId _opCity _requestorId' _requestorRole req = do
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  void $ Domain.Action.UI.TicketDashboard.postUpsertTicketPlaceDashboardDetails (m.id, moCity.id) req _requestorId' _requestorRole
  return Kernel.Types.APISuccess.Success

getTicketsTicketdashboardTicketplaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
  Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketsTicketdashboardTicketplaces _merchantShortId _opCity _status _requestorId _requestorRole = do
  status <- _status & fromMaybeM (InvalidRequest "Status query param is required")
  Domain.Action.UI.TicketDashboard.getTicketPlaceDashboardList status _requestorId _requestorRole

getTicketsTicketdashboardTicketplaceSubPlaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Environment.Flow [Domain.Types.TicketSubPlace.TicketSubPlace]
getTicketsTicketdashboardTicketplaceSubPlaces _merchantShortId _opCity ticketPlaceId = do
  Domain.Action.UI.TicketDashboard.getTicketPlaceDashboardSubPlaces ticketPlaceId

postTicketsTicketdashboardTicketplaceUpdateSubPlaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  [Domain.Types.TicketSubPlace.TicketSubPlace] ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketsTicketdashboardTicketplaceUpdateSubPlaces _merchantShortId _opCity ticketPlaceId req = do
  Domain.Action.UI.TicketDashboard.postUpsertTicketPlaceDashboardSubPlaces ticketPlaceId req

postTicketBookingsVerifyV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketService.TicketService ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService ->
  API.Types.UI.TicketService.TicketServiceVerificationReq ->
  Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp
postTicketBookingsVerifyV2 merchantShortId _opCity personServiceId ticketBookingServiceShortId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingsVerifyV2 (Nothing, m.id) personServiceId ticketBookingServiceShortId req

-- New dashboard handlers for ticket booking flow

postTicketPlacesBook ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  API.Types.UI.TicketService.TicketBookingReq ->
  Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp
postTicketPlacesBook merchantShortId _opCity placeId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketPlacesBook (Nothing, m.id) placeId req

getTicketPlaces ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces merchantShortId _opCity = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlaces (Nothing, m.id)

getTicketPlaceServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) ->
  Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
getTicketPlaceServices merchantShortId _opCity placeId date subPlaceId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlacesServices (Nothing, m.id) placeId date subPlaceId

getTicketBookingDetails ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
  Environment.Flow API.Types.UI.TicketService.TicketBookingDetails
getTicketBookingDetails merchantShortId _opCity bookingShortId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketBookingsDetails (Nothing, m.id) bookingShortId

getTicketsDashboardBookingStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
  Environment.Flow Domain.Types.TicketBooking.BookingStatus
getTicketsDashboardBookingStatus merchantShortId _opCity userPhoneNumber bookingShortId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketsDashboardBookingStatus (Nothing, m.id) userPhoneNumber bookingShortId

getAllTicketBookings ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Domain.Types.TicketBooking.BookingStatus ->
  Environment.Flow [API.Types.UI.TicketService.TicketBookingAPIEntityV2]
getAllTicketBookings merchantShortId _opCity mbLimit mbOffset mbStatus = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketBookingsV2 (Nothing, m.id) mbLimit mbOffset mbStatus

postTicketBookingCashCollect ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketBookingCashCollect merchantShortId _opCity bookingShortId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketBookingsCashCollect (Nothing, m.id) bookingShortId

postTicketPlacesDirectBook ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Maybe Text ->
  API.Types.UI.TicketService.DirectTicketBookingReq ->
  Environment.Flow API.Types.UI.TicketService.DirectTicketBookingResp
postTicketPlacesDirectBook merchantShortId _opCity placeId requestorId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.postTicketPlacesDirectBook (Nothing, m.id) requestorId placeId req

getTicketFleetVehiclesV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Environment.Flow [API.Types.UI.TicketService.TicketFleetVehicleResp]
getTicketFleetVehiclesV2 merchantShortId _opCity placeId mbLimit mbOffset mbSearchString = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketFleetVehiclesV2 (Nothing, m.id) placeId mbLimit mbOffset mbSearchString

getTicketPlaceBookings ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Maybe Int ->
  Maybe Int ->
  Domain.Types.TicketBooking.BookingStatus ->
  Environment.Flow API.Types.UI.TicketService.TicketPlaceBookingList
getTicketPlaceBookings merchantShortId _opCity placeId mbLimit mbOffset status = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.TicketService.getTicketPlaceBookings (Nothing, m.id) placeId mbLimit mbOffset status
