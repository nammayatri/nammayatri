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
import qualified "rider-app" Domain.Types.TicketBooking
import qualified "rider-app" Domain.Types.TicketBookingService
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
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

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTicketsVerify merchantId city :<|> postTicketsServices merchantId city :<|> getTicketsPlaces merchantId city :<|> postTicketsUpdate merchantId city :<|> postTicketsBookingsCancel merchantId city :<|> postTicketsServiceCancel merchantId city :<|> getTicketsBookingDetails merchantId city

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

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId

postTicketsServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Tickets.postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date

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
