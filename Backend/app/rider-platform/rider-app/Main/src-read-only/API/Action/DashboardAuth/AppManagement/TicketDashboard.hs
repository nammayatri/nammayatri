{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TicketDashboard
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified Domain.Action.Dashboard.AppManagement.TicketDashboard
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.TicketPlace
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = (TicketDashboardUploadAsset :<|> TicketDashboardDeleteAsset :<|> TicketDashboardCurrentSeatStatus :<|> TicketDashboardSeatManagement)

type TicketDashboardUploadAsset =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_UPLOAD_ASSET"
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardUploadAsset
  )

type TicketDashboardDeleteAsset =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_DELETE_ASSET"
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardDeleteAsset
  )

type TicketDashboardCurrentSeatStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_CURRENT_SEAT_STATUS"
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardCurrentSeatStatus
  )

type TicketDashboardSeatManagement =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_SEAT_MANAGEMENT"
      :> API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardSeatManagement
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = ticketDashboardUploadAsset merchantId city :<|> ticketDashboardDeleteAsset merchantId city :<|> ticketDashboardCurrentSeatStatus merchantId city :<|> ticketDashboardSeatManagement merchantId city

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardUploadAsset a5 a4 a2 a1

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardDeleteAsset a5 a4 a2 a1

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardCurrentSeatStatus a5 a4 a2 a1

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardSeatManagement a5 a4 a2 a1
