{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.TicketDashboard
  ( API.Types.Dashboard.AppManagement.TicketDashboard.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified Domain.Action.Dashboard.AppManagement.TicketDashboard
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.TicketPlace
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.TicketDashboard.API)
handler merchantId city = ticketDashboardUploadAsset merchantId city :<|> ticketDashboardDeleteAsset merchantId city :<|> ticketDashboardCurrentSeatStatus merchantId city :<|> ticketDashboardSeatManagement merchantId city

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardUploadAsset a4 a3 a2 a1

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardDeleteAsset a4 a3 a2 a1

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardCurrentSeatStatus a4 a3 a2 a1

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TicketDashboard.ticketDashboardSeatManagement a4 a3 a2 a1
