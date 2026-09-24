{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.RadarTickets
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.RadarTickets
import qualified Domain.Action.ProviderPlatform.Management.RadarTickets
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()

type API = ("radarTickets" :> (GetRadarTicketsList :<|> GetRadarTicketsSummary :<|> GetRadarTicketsReporters :<|> GetRadarTicketsTicket :<|> GetRadarTicketsTicketConversation :<|> GetRadarTicketsTicketAttachment :<|> PostRadarTicketsTicketReply :<|> PostRadarTicketsUpload :<|> PostRadarTicketsCreate :<|> PostRadarTicketsTicketUpdateStatus :<|> PostRadarTicketsTicketCsat))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRadarTicketsList merchantId city :<|> getRadarTicketsSummary merchantId city :<|> getRadarTicketsReporters merchantId city :<|> getRadarTicketsTicket merchantId city :<|> getRadarTicketsTicketConversation merchantId city :<|> getRadarTicketsTicketAttachment merchantId city :<|> postRadarTicketsTicketReply merchantId city :<|> postRadarTicketsUpload merchantId city :<|> postRadarTicketsCreate merchantId city :<|> postRadarTicketsTicketUpdateStatus merchantId city :<|> postRadarTicketsTicketCsat merchantId city

type GetRadarTicketsList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_LIST))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsList
  )

type GetRadarTicketsSummary =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_SUMMARY))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsSummary
  )

type GetRadarTicketsReporters =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_REPORTERS))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsReporters
  )

type GetRadarTicketsTicket =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_TICKET))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicket
  )

type GetRadarTicketsTicketConversation =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_TICKET_CONVERSATION))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicketConversation
  )

type GetRadarTicketsTicketAttachment =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.GET_RADAR_TICKETS_TICKET_ATTACHMENT))
      :> API.Types.ProviderPlatform.Management.RadarTickets.GetRadarTicketsTicketAttachment
  )

type PostRadarTicketsTicketReply =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.POST_RADAR_TICKETS_TICKET_REPLY))
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketReply
  )

type PostRadarTicketsUpload =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.POST_RADAR_TICKETS_UPLOAD))
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsUpload
  )

type PostRadarTicketsCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.POST_RADAR_TICKETS_CREATE))
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsCreate
  )

type PostRadarTicketsTicketUpdateStatus =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.POST_RADAR_TICKETS_TICKET_UPDATE_STATUS))
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketUpdateStatus
  )

type PostRadarTicketsTicketCsat =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RADAR_TICKETS) / ('API.Types.ProviderPlatform.Management.RadarTickets.POST_RADAR_TICKETS_TICKET_CSAT))
      :> API.Types.ProviderPlatform.Management.RadarTickets.PostRadarTicketsTicketCsat
  )

getRadarTicketsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketListRes)
getRadarTicketsList merchantShortId opCity apiTokenInfo pageSize cursor priority stageName reporterEmail = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsList merchantShortId opCity apiTokenInfo pageSize cursor priority stageName reporterEmail

getRadarTicketsSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarSummaryRes)
getRadarTicketsSummary merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsSummary merchantShortId opCity apiTokenInfo

getRadarTicketsReporters :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarReportersRes)
getRadarTicketsReporters merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsReporters merchantShortId opCity apiTokenInfo

getRadarTicketsTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketDetailRes)
getRadarTicketsTicket merchantShortId opCity apiTokenInfo ticketId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsTicket merchantShortId opCity apiTokenInfo ticketId

getRadarTicketsTicketConversation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarConversationRes)
getRadarTicketsTicketConversation merchantShortId opCity apiTokenInfo ticketId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsTicketConversation merchantShortId opCity apiTokenInfo ticketId

getRadarTicketsTicketAttachment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarAttachmentUrlRes)
getRadarTicketsTicketAttachment merchantShortId opCity apiTokenInfo ticketId attachmentId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.getRadarTicketsTicketAttachment merchantShortId opCity apiTokenInfo ticketId attachmentId

postRadarTicketsTicketReply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketReply merchantShortId opCity apiTokenInfo ticketId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.postRadarTicketsTicketReply merchantShortId opCity apiTokenInfo ticketId req

postRadarTicketsUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadRes)
postRadarTicketsUpload merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.postRadarTicketsUpload merchantShortId opCity apiTokenInfo req

postRadarTicketsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketRes)
postRadarTicketsCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.postRadarTicketsCreate merchantShortId opCity apiTokenInfo req

postRadarTicketsTicketUpdateStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUpdateStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketUpdateStatus merchantShortId opCity apiTokenInfo ticketId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.postRadarTicketsTicketUpdateStatus merchantShortId opCity apiTokenInfo ticketId req

postRadarTicketsTicketCsat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCsatReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketCsat merchantShortId opCity apiTokenInfo ticketId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RadarTickets.postRadarTicketsTicketCsat merchantShortId opCity apiTokenInfo ticketId req
