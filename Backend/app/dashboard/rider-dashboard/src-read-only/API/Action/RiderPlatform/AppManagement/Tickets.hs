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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails :<|> PostTicketsTicketdashboardRegister :<|> PostTicketsTicketdashboardLoginAuth :<|> PostTicketsTicketdashboardLoginVerify :<|> GetTicketsTicketdashboardAgreement :<|> GetTicketsTicketdashboardUserInfo :<|> GetTicketsTicketdashboardFile :<|> PostTicketsTicketdashboardSendverifyotp :<|> GetTicketsTicketdashboardTicketplaceInfo :<|> PostTicketsTicketdashboardTicketplaceUpdate :<|> GetTicketsTicketdashboardTicketplaces :<|> GetTicketFleetVehicles)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTicketsVerify merchantId city :<|> postTicketsServices merchantId city :<|> getTicketsPlaces merchantId city :<|> postTicketsUpdate merchantId city :<|> postTicketsBookingsCancel merchantId city :<|> postTicketsServiceCancel merchantId city :<|> getTicketsBookingDetails merchantId city :<|> postTicketsTicketdashboardRegister merchantId city :<|> postTicketsTicketdashboardLoginAuth merchantId city :<|> postTicketsTicketdashboardLoginVerify merchantId city :<|> getTicketsTicketdashboardAgreement merchantId city :<|> getTicketsTicketdashboardUserInfo merchantId city :<|> getTicketsTicketdashboardFile merchantId city :<|> postTicketsTicketdashboardSendverifyotp merchantId city :<|> getTicketsTicketdashboardTicketplaceInfo merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdate merchantId city :<|> getTicketsTicketdashboardTicketplaces merchantId city :<|> getTicketFleetVehicles merchantId city

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

type GetTicketFleetVehicles =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKETS / 'API.Types.Dashboard.AppManagement.Tickets.GET_TICKET_FLEET_VEHICLES)
      :> API.Types.Dashboard.AppManagement.Tickets.GetTicketFleetVehicles
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

getTicketFleetVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp])
getTicketFleetVehicles merchantShortId opCity apiTokenInfo placeId limit offset searchString = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.getTicketFleetVehicles merchantShortId opCity apiTokenInfo placeId limit offset searchString
