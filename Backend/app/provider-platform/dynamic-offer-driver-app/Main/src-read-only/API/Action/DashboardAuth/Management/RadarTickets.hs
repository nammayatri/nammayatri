{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RadarTickets
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("radarTickets" :> (GetRadarTicketsList :<|> GetRadarTicketsSummary :<|> GetRadarTicketsReporters :<|> GetRadarTicketsTicket :<|> GetRadarTicketsTicketConversation :<|> GetRadarTicketsTicketAttachment :<|> PostRadarTicketsTicketReply :<|> PostRadarTicketsUpload :<|> PostRadarTicketsCreate :<|> PostRadarTicketsTicketUpdateStatus :<|> PostRadarTicketsTicketCsat))

type GetRadarTicketsList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_LIST"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsList
  )

type GetRadarTicketsSummary =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_SUMMARY"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsSummary
  )

type GetRadarTicketsReporters =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_REPORTERS"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsReporters
  )

type GetRadarTicketsTicket =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicket
  )

type GetRadarTicketsTicketConversation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET_CONVERSATION"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicketConversation
  )

type GetRadarTicketsTicketAttachment =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET_ATTACHMENT"
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicketAttachment
  )

type PostRadarTicketsTicketReply =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_REPLY"
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketReply
  )

type PostRadarTicketsUpload =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_UPLOAD"
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsUpload
  )

type PostRadarTicketsCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_CREATE"
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsCreate
  )

type PostRadarTicketsTicketUpdateStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_UPDATE_STATUS"
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketUpdateStatus
  )

type PostRadarTicketsTicketCsat =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_CSAT"
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketCsat
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRadarTicketsList merchantId city :<|> getRadarTicketsSummary merchantId city :<|> getRadarTicketsReporters merchantId city :<|> getRadarTicketsTicket merchantId city :<|> getRadarTicketsTicketConversation merchantId city :<|> getRadarTicketsTicketAttachment merchantId city :<|> postRadarTicketsTicketReply merchantId city :<|> postRadarTicketsUpload merchantId city :<|> postRadarTicketsCreate merchantId city :<|> postRadarTicketsTicketUpdateStatus merchantId city :<|> postRadarTicketsTicketCsat merchantId city

getRadarTicketsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketListRes)
getRadarTicketsList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsList a8 a7 a5 a4 a3 a2 a1

getRadarTicketsSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarSummaryRes)
getRadarTicketsSummary a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsSummary a3 a2

getRadarTicketsReporters :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarReportersRes)
getRadarTicketsReporters a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsReporters a3 a2

getRadarTicketsTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketDetailRes)
getRadarTicketsTicket a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicket a4 a3 a1

getRadarTicketsTicketConversation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarConversationRes)
getRadarTicketsTicketConversation a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicketConversation a4 a3 a1

getRadarTicketsTicketAttachment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarAttachmentUrlRes)
getRadarTicketsTicketAttachment a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RadarTickets.getRadarTicketsTicketAttachment a5 a4 a2 a1

postRadarTicketsTicketReply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketReply a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_REPLY" a3 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketReply a5 a4 a2 a1
    )

postRadarTicketsUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadRes)
postRadarTicketsUpload a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_UPLOAD" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsUpload a4 a3 a1
    )

postRadarTicketsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketRes)
postRadarTicketsCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsCreate a4 a3 a1
    )

postRadarTicketsTicketUpdateStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUpdateStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketUpdateStatus a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_UPDATE_STATUS" a3 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketUpdateStatus a5 a4 a2 a1
    )

postRadarTicketsTicketCsat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCsatReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketCsat a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_CSAT" a3 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Management.RadarTickets.postRadarTicketsTicketCsat a5 a4 a2 a1
    )
