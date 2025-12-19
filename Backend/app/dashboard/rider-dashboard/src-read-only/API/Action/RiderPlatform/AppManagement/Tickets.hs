{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Tickets
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Tickets
import qualified "rider-app" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.RiderPlatform.AppManagement.Tickets
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.TicketBooking
import qualified "rider-app" Domain.Types.TicketBookingService
import qualified "rider-app" Domain.Types.TicketDashboard
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
import qualified Domain.Types.TicketSubPlace
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails :<|> PostTicketsTicketdashboardRegister :<|> PostTicketsTicketdashboardLoginAuth :<|> PostTicketsTicketdashboardLoginVerify :<|> GetTicketsTicketdashboardAgreement :<|> GetTicketsTicketdashboardUserInfo :<|> GetTicketsTicketdashboardFile :<|> PostTicketsTicketdashboardSendverifyotp :<|> GetTicketsTicketdashboardTicketplaceInfo :<|> PostTicketsTicketdashboardTicketplaceUpdate :<|> GetTicketsTicketdashboardTicketplaces :<|> GetTicketsTicketdashboardTicketplaceSubPlaces :<|> PostTicketsTicketdashboardTicketplaceUpdateSubPlaces :<|> GetTicketFleetVehicles :<|> GetTicketFleetVehiclesV2 :<|> PostTicketBookingsVerifyV2 :<|> PostTicketPlacesBook :<|> GetTicketPlaces :<|> GetTicketPlaceServices :<|> GetTicketBookingDetails :<|> GetTicketsDashboardBookingStatus :<|> GetAllTicketBookings :<|> PostTicketBookingCashCollect :<|> PostTicketPlacesDirectBook :<|> GetTicketPlaceBookings)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTicketsVerify merchantId city :<|> postTicketsServices merchantId city :<|> getTicketsPlaces merchantId city :<|> postTicketsUpdate merchantId city :<|> postTicketsBookingsCancel merchantId city :<|> postTicketsServiceCancel merchantId city :<|> getTicketsBookingDetails merchantId city :<|> postTicketsTicketdashboardRegister merchantId city :<|> postTicketsTicketdashboardLoginAuth merchantId city :<|> postTicketsTicketdashboardLoginVerify merchantId city :<|> getTicketsTicketdashboardAgreement merchantId city :<|> getTicketsTicketdashboardUserInfo merchantId city :<|> getTicketsTicketdashboardFile merchantId city :<|> postTicketsTicketdashboardSendverifyotp merchantId city :<|> getTicketsTicketdashboardTicketplaceInfo merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdate merchantId city :<|> getTicketsTicketdashboardTicketplaces merchantId city :<|> getTicketsTicketdashboardTicketplaceSubPlaces merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdateSubPlaces merchantId city :<|> getTicketFleetVehicles merchantId city :<|> getTicketFleetVehiclesV2 merchantId city :<|> postTicketBookingsVerifyV2 merchantId city :<|> postTicketPlacesBook merchantId city :<|> getTicketPlaces merchantId city :<|> getTicketPlaceServices merchantId city :<|> getTicketBookingDetails merchantId city :<|> getTicketsDashboardBookingStatus merchantId city :<|> getAllTicketBookings merchantId city :<|> postTicketBookingCashCollect merchantId city :<|> postTicketPlacesDirectBook merchantId city :<|> getTicketPlaceBookings merchantId city

type PostTicketsVerify =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_VERIFY)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsVerify
  )

type PostTicketsServices =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_SERVICES)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsServices
  )

type GetTicketsPlaces =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_PLACES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsPlaces
  )

type PostTicketsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_UPDATE)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsUpdate
  )

type PostTicketsBookingsCancel =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_BOOKINGS_CANCEL)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsBookingsCancel
  )

type PostTicketsServiceCancel =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_SERVICE_CANCEL)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsServiceCancel
  )

type GetTicketsBookingDetails =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_BOOKING_DETAILS)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsBookingDetails
  )

type PostTicketsTicketdashboardRegister = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardRegister

type PostTicketsTicketdashboardLoginAuth = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardLoginAuth

type PostTicketsTicketdashboardLoginVerify = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardLoginVerify

type GetTicketsTicketdashboardAgreement =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_AGREEMENT)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardAgreement
  )

type GetTicketsTicketdashboardUserInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_USER_INFO)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardUserInfo
  )

type GetTicketsTicketdashboardFile =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_FILE)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardFile
  )

type PostTicketsTicketdashboardSendverifyotp = API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardSendverifyotp

type GetTicketsTicketdashboardTicketplaceInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaceInfo
  )

type PostTicketsTicketdashboardTicketplaceUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardTicketplaceUpdate
  )

type GetTicketsTicketdashboardTicketplaces =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_TICKETPLACES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaces
  )

type GetTicketsTicketdashboardTicketplaceSubPlaces =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_SUB_PLACES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsTicketdashboardTicketplaceSubPlaces
  )

type PostTicketsTicketdashboardTicketplaceUpdateSubPlaces =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE_SUB_PLACES)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketsTicketdashboardTicketplaceUpdateSubPlaces
  )

type GetTicketFleetVehicles =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_FLEET_VEHICLES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketFleetVehicles
  )

type GetTicketFleetVehiclesV2 =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_FLEET_VEHICLES_V2)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketFleetVehiclesV2
  )

type PostTicketBookingsVerifyV2 =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKET_BOOKINGS_VERIFY_V2)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketBookingsVerifyV2
  )

type PostTicketPlacesBook =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKET_PLACES_BOOK)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketPlacesBook
  )

type GetTicketPlaces =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_PLACES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaces
  )

type GetTicketPlaceServices =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_PLACE_SERVICES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaceServices
  )

type GetTicketBookingDetails =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_BOOKING_DETAILS)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketBookingDetails
  )

type GetTicketsDashboardBookingStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKETS_DASHBOARD_BOOKING_STATUS)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketsDashboardBookingStatus
  )

type GetAllTicketBookings =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_ALL_TICKET_BOOKINGS)
      :> API.Types.Dashboard.AppManagement.Tickets.GetAllTicketBookings
  )

type PostTicketBookingCashCollect =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKET_BOOKING_CASH_COLLECT)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketBookingCashCollect
  )

type PostTicketPlacesDirectBook =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.POST_TICKET_PLACES_DIRECT_BOOK)
      :> API.Types.Dashboard.AppManagement.Tickets.PostTicketPlacesDirectBook
  )

type GetTicketPlaceBookings =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_PLACE_BOOKINGS)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketPlaceBookings
  )

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId fleetOwnerId vehicleId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId fleetOwnerId vehicleId

postTicketsServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date subPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date subPlaceId

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsPlaces merchantShortId opCity apiTokenInfo

postTicketsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsUpdate merchantShortId opCity apiTokenInfo req

postTicketsBookingsCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsBookingsCancel merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsBookingsCancel merchantShortId opCity apiTokenInfo req

postTicketsServiceCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsServiceCancel merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsServiceCancel merchantShortId opCity apiTokenInfo req

getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails)
getTicketsBookingDetails merchantShortId opCity apiTokenInfo ticketBookingShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsBookingDetails merchantShortId opCity apiTokenInfo ticketBookingShortId

postTicketsTicketdashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp)
postTicketsTicketdashboardRegister merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardRegister merchantShortId opCity req

postTicketsTicketdashboardLoginAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardLoginAuth merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardLoginAuth merchantShortId opCity req

postTicketsTicketdashboardLoginVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp)
postTicketsTicketdashboardLoginVerify merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardLoginVerify merchantShortId opCity req

getTicketsTicketdashboardAgreement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp)
getTicketsTicketdashboardAgreement merchantShortId opCity apiTokenInfo templateName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardAgreement merchantShortId opCity apiTokenInfo templateName

getTicketsTicketdashboardUserInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardUserInfo)
getTicketsTicketdashboardUserInfo merchantShortId opCity apiTokenInfo requestorId userRole requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardUserInfo merchantShortId opCity apiTokenInfo requestorId userRole requestorRole

getTicketsTicketdashboardFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
getTicketsTicketdashboardFile merchantShortId opCity apiTokenInfo fileId requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardFile merchantShortId opCity apiTokenInfo fileId requestorId requestorRole

postTicketsTicketdashboardSendverifyotp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.SendVerifyOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardSendverifyotp merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardSendverifyotp merchantShortId opCity req

getTicketsTicketdashboardTicketplaceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.TicketDashboard.TicketPlaceDashboardDetails)
getTicketsTicketdashboardTicketplaceInfo merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardTicketplaceInfo merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole

postTicketsTicketdashboardTicketplaceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.TicketDashboard.TicketPlaceDashboardDetails -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdate merchantShortId opCity apiTokenInfo requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardTicketplaceUpdate merchantShortId opCity apiTokenInfo requestorId requestorRole req

getTicketsTicketdashboardTicketplaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsTicketdashboardTicketplaces merchantShortId opCity apiTokenInfo status requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardTicketplaces merchantShortId opCity apiTokenInfo status requestorId requestorRole

getTicketsTicketdashboardTicketplaceSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler [Domain.Types.TicketSubPlace.TicketSubPlace])
getTicketsTicketdashboardTicketplaceSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsTicketdashboardTicketplaceSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId

postTicketsTicketdashboardTicketplaceUpdateSubPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> [Domain.Types.TicketSubPlace.TicketSubPlace] -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdateSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsTicketdashboardTicketplaceUpdateSubPlaces merchantShortId opCity apiTokenInfo ticketPlaceId req

getTicketFleetVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp])
getTicketFleetVehicles merchantShortId opCity apiTokenInfo placeId limit offset searchString = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketFleetVehicles merchantShortId opCity apiTokenInfo placeId limit offset searchString

getTicketFleetVehiclesV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp])
getTicketFleetVehiclesV2 merchantShortId opCity apiTokenInfo placeId limit offset searchString = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketFleetVehiclesV2 merchantShortId opCity apiTokenInfo placeId limit offset searchString

postTicketBookingsVerifyV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> API.Types.UI.TicketService.TicketServiceVerificationReq -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketBookingsVerifyV2 merchantShortId opCity apiTokenInfo personServiceId ticketServiceShortId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketBookingsVerifyV2 merchantShortId opCity apiTokenInfo personServiceId ticketServiceShortId req

postTicketPlacesBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp)
postTicketPlacesBook merchantShortId opCity apiTokenInfo placeId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketPlacesBook merchantShortId opCity apiTokenInfo placeId req

getTicketPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketPlaces merchantShortId opCity apiTokenInfo

getTicketPlaceServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
getTicketPlaceServices merchantShortId opCity apiTokenInfo placeId date subPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketPlaceServices merchantShortId opCity apiTokenInfo placeId date subPlaceId

getTicketBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails)
getTicketBookingDetails merchantShortId opCity apiTokenInfo bookingShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketBookingDetails merchantShortId opCity apiTokenInfo bookingShortId

getTicketsDashboardBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Domain.Types.TicketBooking.BookingStatus)
getTicketsDashboardBookingStatus merchantShortId opCity apiTokenInfo userPhoneNumber bookingShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketsDashboardBookingStatus merchantShortId opCity apiTokenInfo userPhoneNumber bookingShortId

getAllTicketBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.TicketBooking.BookingStatus -> Environment.FlowHandler [API.Types.UI.TicketService.TicketBookingAPIEntityV2])
getAllTicketBookings merchantShortId opCity apiTokenInfo limit offset status = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getAllTicketBookings merchantShortId opCity apiTokenInfo limit offset status

postTicketBookingCashCollect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketBookingCashCollect merchantShortId opCity apiTokenInfo bookingShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketBookingCashCollect merchantShortId opCity apiTokenInfo bookingShortId

postTicketPlacesDirectBook :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.TicketService.DirectTicketBookingReq -> Environment.FlowHandler API.Types.UI.TicketService.DirectTicketBookingResp)
postTicketPlacesDirectBook merchantShortId opCity apiTokenInfo placeId requestorId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketPlacesDirectBook merchantShortId opCity apiTokenInfo placeId requestorId req

getTicketPlaceBookings :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Domain.Types.TicketBooking.BookingStatus -> Environment.FlowHandler API.Types.UI.TicketService.TicketPlaceBookingList
getTicketPlaceBookings merchantShortId opCity apiTokenInfo placeId limit offset status = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketPlaceBookings merchantShortId opCity apiTokenInfo placeId limit offset status
