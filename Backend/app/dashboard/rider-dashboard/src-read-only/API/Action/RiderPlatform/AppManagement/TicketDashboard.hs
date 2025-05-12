{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.TicketDashboard
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.TicketDashboard
import qualified Domain.Action.RiderPlatform.AppManagement.TicketDashboard
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.TicketPlace
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

type API = (TicketDashboardUploadAsset :<|> TicketDashboardDeleteAsset :<|> TicketDashboardCurrentSeatStatus :<|> TicketDashboardSeatManagement)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = ticketDashboardUploadAsset merchantId city :<|> ticketDashboardDeleteAsset merchantId city :<|> ticketDashboardCurrentSeatStatus merchantId city :<|> ticketDashboardSeatManagement merchantId city

type TicketDashboardUploadAsset =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKET_DASHBOARD / 'API.Types.Dashboard.AppManagement.TicketDashboard.TICKET_DASHBOARD_UPLOAD_ASSET)
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardUploadAsset
  )

type TicketDashboardDeleteAsset =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKET_DASHBOARD / 'API.Types.Dashboard.AppManagement.TicketDashboard.TICKET_DASHBOARD_DELETE_ASSET)
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardDeleteAsset
  )

type TicketDashboardCurrentSeatStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKET_DASHBOARD / 'API.Types.Dashboard.AppManagement.TicketDashboard.TICKET_DASHBOARD_CURRENT_SEAT_STATUS)
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardCurrentSeatStatus
  )

type TicketDashboardSeatManagement =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TICKET_DASHBOARD / 'API.Types.Dashboard.AppManagement.TicketDashboard.TICKET_DASHBOARD_SEAT_MANAGEMENT)
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardSeatManagement
  )

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TicketDashboard.ticketDashboardUploadAsset merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TicketDashboard.ticketDashboardDeleteAsset merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TicketDashboard.ticketDashboardCurrentSeatStatus merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TicketDashboard.ticketDashboardSeatManagement merchantShortId opCity apiTokenInfo ticketPlaceId requestorId requestorRole req
