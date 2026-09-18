{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.RadarTickets
  ( API.Types.ProviderPlatform.Management.RadarTickets.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.RadarTickets
import qualified Domain.Action.Dashboard.Management.RadarTickets
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.RadarTickets.API)
handler merchantId city = getRadarTicketsList merchantId city :<|> getRadarTicketsSummary merchantId city :<|> getRadarTicketsReporters merchantId city :<|> getRadarTicketsTicket merchantId city :<|> getRadarTicketsTicketAttachment merchantId city :<|> getRadarTicketsTicketConversation merchantId city :<|> postRadarTicketsTicketReply merchantId city :<|> postRadarTicketsCreate merchantId city :<|> postRadarTicketsTicketUpdateStatus merchantId city :<|> postRadarTicketsTicketCsat merchantId city

getRadarTicketsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketListRes)
getRadarTicketsList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsList a7 a6 a5 a4 a3 a2 a1

getRadarTicketsSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarSummaryRes)
getRadarTicketsSummary a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsSummary a2 a1

getRadarTicketsReporters :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarReportersRes)
getRadarTicketsReporters a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsReporters a2 a1

getRadarTicketsTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketDetailRes)
getRadarTicketsTicket a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicket a3 a2 a1

getRadarTicketsTicketAttachment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarAttachmentUrlRes)
getRadarTicketsTicketAttachment a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicketAttachment a4 a3 a2 a1

getRadarTicketsTicketConversation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarConversationRes)
getRadarTicketsTicketConversation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicketConversation a3 a2 a1

postRadarTicketsTicketReply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketReply a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketReply a4 a3 a2 a1

postRadarTicketsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketRes)
postRadarTicketsCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsCreate a3 a2 a1

postRadarTicketsTicketUpdateStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUpdateStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketUpdateStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketUpdateStatus a4 a3 a2 a1

postRadarTicketsTicketCsat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCsatReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketCsat a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketCsat a4 a3 a2 a1
