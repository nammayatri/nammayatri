{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Tickets
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified "this" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.Dashboard.AppManagement.Tickets
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketDashboard
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Domain.Types.TicketSubPlace
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails :<|> PostTicketsTicketdashboardRegister :<|> PostTicketsTicketdashboardLoginAuth :<|> PostTicketsTicketdashboardLoginVerify :<|> GetTicketsTicketdashboardAgreement :<|> GetTicketsTicketdashboardUserInfo :<|> GetTicketsTicketdashboardFile :<|> PostTicketsTicketdashboardSendverifyotp :<|> GetTicketsTicketdashboardTicketplaceInfo :<|> PostTicketsTicketdashboardTicketplaceUpdate :<|> GetTicketsTicketdashboardTicketplaces :<|> GetTicketsTicketdashboardTicketplaceSubPlaces :<|> PostTicketsTicketdashboardTicketplaceUpdateSubPlaces :<|> GetTicketFleetVehicles :<|> GetTicketFleetVehiclesV2 :<|> PostTicketBookingsVerifyV2 :<|> PostTicketPlacesBook :<|> GetTicketPlaces :<|> GetTicketPlaceServices :<|> GetTicketBookingDetails :<|> GetTicketsDashboardBookingStatus :<|> GetAllTicketBookings :<|> PostTicketBookingCashCollect :<|> PostTicketPlacesDirectBook :<|> GetTicketPlaceBookings)

type PostTicketsVerify = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_VERIFY" :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsVerify)

type PostTicketsServices = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICES" :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsServices)

type GetTicketsPlaces = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_PLACES" :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsPlaces)

type PostTicketsUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_UPDATE" :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsUpdate)

type PostTicketsBookingsCancel =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_BOOKINGS_CANCEL"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsBookingsCancel
  )

type PostTicketsServiceCancel =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICE_CANCEL"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsServiceCancel
  )

type GetTicketsBookingDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_BOOKING_DETAILS"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsBookingDetails
  )

type PostTicketsTicketdashboardRegister = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardRegister

type PostTicketsTicketdashboardLoginAuth = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardLoginAuth

type PostTicketsTicketdashboardLoginVerify = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardLoginVerify

type GetTicketsTicketdashboardAgreement =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardAgreement
  )

type GetTicketsTicketdashboardUserInfo =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardUserInfo
  )

type GetTicketsTicketdashboardFile =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardFile
  )

type PostTicketsTicketdashboardSendverifyotp = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardSendverifyotp

type GetTicketsTicketdashboardTicketplaceInfo =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaceInfo
  )

type PostTicketsTicketdashboardTicketplaceUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardTicketplaceUpdate
  )

type GetTicketsTicketdashboardTicketplaces =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACES"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaces
  )

type GetTicketsTicketdashboardTicketplaceSubPlaces =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_SUB_PLACES"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaceSubPlaces
  )

type PostTicketsTicketdashboardTicketplaceUpdateSubPlaces =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE_SUB_PLACES"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardTicketplaceUpdateSubPlaces
  )

type GetTicketFleetVehicles = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_FLEET_VEHICLES" :> API.Types.Dashboard.AppManagement.Tickets.GetTicketFleetVehicles)

type GetTicketFleetVehiclesV2 =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_FLEET_VEHICLES_V2"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketFleetVehiclesV2
  )

type PostTicketBookingsVerifyV2 =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_BOOKINGS_VERIFY_V2"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketBookingsVerifyV2
  )

type PostTicketPlacesBook =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_PLACES_BOOK"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketPlacesBook
  )

type GetTicketPlaces = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACES" :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaces)

type GetTicketPlaceServices = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACE_SERVICES" :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaceServices)

type GetTicketBookingDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_BOOKING_DETAILS"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketBookingDetails
  )

type GetTicketsDashboardBookingStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_DASHBOARD_BOOKING_STATUS"
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsDashboardBookingStatus
  )

type GetAllTicketBookings = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_ALL_TICKET_BOOKINGS" :> API.Types.Dashboard.AppManagement.Tickets.GetAllTicketBookings)

type PostTicketBookingCashCollect =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_BOOKING_CASH_COLLECT"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketBookingCashCollect
  )

type PostTicketPlacesDirectBook =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_PLACES_DIRECT_BOOK"
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketPlacesDirectBook
  )

type GetTicketPlaceBookings = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACE_BOOKINGS" :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaceBookings)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTicketsVerify merchantId city :<|> postTicketsServices merchantId city :<|> getTicketsPlaces merchantId city :<|> postTicketsUpdate merchantId city :<|> postTicketsBookingsCancel merchantId city :<|> postTicketsServiceCancel merchantId city :<|> getTicketsBookingDetails merchantId city :<|> postTicketsTicketdashboardRegister merchantId city :<|> postTicketsTicketdashboardLoginAuth merchantId city :<|> postTicketsTicketdashboardLoginVerify merchantId city :<|> getTicketsTicketdashboardAgreement merchantId city :<|> getTicketsTicketdashboardUserInfo merchantId city :<|> getTicketsTicketdashboardFile merchantId city :<|> postTicketsTicketdashboardSendverifyotp merchantId city :<|> getTicketsTicketdashboardTicketplaceInfo merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdate merchantId city :<|> getTicketsTicketdashboardTicketplaces merchantId city :<|> getTicketsTicketdashboardTicketplaceSubPlaces merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdateSubPlaces merchantId city :<|> getTicketFleetVehicles merchantId city :<|> getTicketFleetVehiclesV2 merchantId city :<|> postTicketBookingsVerifyV2 merchantId city :<|> postTicketPlacesBook merchantId city :<|> getTicketPlaces merchantId city :<|> getTicketPlaceServices merchantId city :<|> getTicketBookingDetails merchantId city :<|> getTicketsDashboardBookingStatus merchantId city :<|> getAllTicketBookings merchantId city :<|> postTicketBookingCashCollect merchantId city :<|> postTicketPlacesDirectBook merchantId city :<|> getTicketPlaceBookings merchantId city

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsVerify a7 a6 a4 a3 a2 a1

postTicketsServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
postTicketsServices a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsServices a6 a5 a3 a2 a1

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsPlaces a3 a2

postTicketsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsUpdate a4 a3 a1

postTicketsBookingsCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsBookingsCancel a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsBookingsCancel a4 a3 a1

postTicketsServiceCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsServiceCancel a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsServiceCancel a4 a3 a1

getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails)
getTicketsBookingDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsBookingDetails a4 a3 a1

postTicketsTicketdashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp)
postTicketsTicketdashboardRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardRegister a3 a2 a1

postTicketsTicketdashboardLoginAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardLoginAuth a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardLoginAuth a3 a2 a1

postTicketsTicketdashboardLoginVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp)
postTicketsTicketdashboardLoginVerify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardLoginVerify a3 a2 a1

getTicketsTicketdashboardAgreement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp)
getTicketsTicketdashboardAgreement a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardAgreement a4 a3 a1

getTicketsTicketdashboardUserInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardUserInfo)
getTicketsTicketdashboardUserInfo a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardUserInfo a6 a5 a3 a2 a1

getTicketsTicketdashboardFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
getTicketsTicketdashboardFile a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardFile a6 a5 a3 a2 a1

postTicketsTicketdashboardSendverifyotp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.SendVerifyOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardSendverifyotp a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardSendverifyotp a3 a2 a1

getTicketsTicketdashboardTicketplaceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Domain.Types.TicketDashboard.TicketPlaceDashboardDetails)
getTicketsTicketdashboardTicketplaceInfo a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardTicketplaceInfo a6 a5 a3 a2 a1

postTicketsTicketdashboardTicketplaceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.TicketDashboard.TicketPlaceDashboardDetails -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardTicketplaceUpdate a6 a5 a3 a2 a1

getTicketsTicketdashboardTicketplaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsTicketdashboardTicketplaces a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardTicketplaces a6 a5 a3 a2 a1

getTicketsTicketdashboardTicketplaceSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler [Domain.Types.TicketSubPlace.TicketSubPlace])
getTicketsTicketdashboardTicketplaceSubPlaces a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardTicketplaceSubPlaces a4 a3 a1

postTicketsTicketdashboardTicketplaceUpdateSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> [Domain.Types.TicketSubPlace.TicketSubPlace] -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdateSubPlaces a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardTicketplaceUpdateSubPlaces a5 a4 a2 a1

getTicketFleetVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp])
getTicketFleetVehicles a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketFleetVehicles a7 a6 a4 a3 a2 a1

getTicketFleetVehiclesV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp])
getTicketFleetVehiclesV2 a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketFleetVehiclesV2 a7 a6 a4 a3 a2 a1

postTicketBookingsVerifyV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> API.Types.UI.TicketService.TicketServiceVerificationReq -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketBookingsVerifyV2 a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketBookingsVerifyV2 a6 a5 a3 a2 a1

postTicketPlacesBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp)
postTicketPlacesBook a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketPlacesBook a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

getTicketPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketPlaces a3 a2

getTicketPlaceServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
getTicketPlaceServices a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketPlaceServices a6 a5 a3 a2 a1

getTicketBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails)
getTicketBookingDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketBookingDetails a4 a3 a1

getTicketsDashboardBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Domain.Types.TicketBooking.BookingStatus)
getTicketsDashboardBookingStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsDashboardBookingStatus a5 a4 a2 a1

getAllTicketBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.TicketBooking.BookingStatus) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketBookingAPIEntityV2])
getAllTicketBookings a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getAllTicketBookings a6 a5 a3 a2 a1

postTicketBookingCashCollect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketBookingCashCollect a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketBookingCashCollect a4 a3 a1

postTicketPlacesDirectBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.TicketService.DirectTicketBookingReq -> Environment.FlowHandler API.Types.UI.TicketService.DirectTicketBookingResp)
postTicketPlacesDirectBook a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketPlacesDirectBook a6 a5 a3 a2 a1

getTicketPlaceBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.FlowHandler API.Types.UI.TicketService.TicketPlaceBookingList)
getTicketPlaceBookings a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketPlaceBookings a7 a6 a4 a3 a2 a1
