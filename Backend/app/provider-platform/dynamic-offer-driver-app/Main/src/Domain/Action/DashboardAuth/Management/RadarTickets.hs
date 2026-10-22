-- | Hand-written handlers for the direct-dashboard RADAR routes. Every endpoint
-- marked @appServerHandler: custom@ in the spec lands here with the verified
-- session's 'DashboardUser', which decides two things the client never does:
--
-- * who the operator is on desk writes — the Helper requests (@*ByUserReq@)
--   carry the sender, filled from the session;
-- * which tickets the operator may see — 'ticketScope'.
module Domain.Action.DashboardAuth.Management.RadarTickets
  ( getRadarTicketsList,
    getRadarTicketsSummary,
    getRadarTicketsReporters,
    getRadarTicketsTicket,
    getRadarTicketsTicketConversation,
    getRadarTicketsTicketAttachment,
    postRadarTicketsTicketReply,
    postRadarTicketsCreate,
    postRadarTicketsTicketUpdateStatus,
    postRadarTicketsTicketCsat,
  )
where

import qualified API.Types.ProviderPlatform.Management.RadarTickets as Common
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Management.RadarTickets as DRadarTickets
import qualified Domain.Types.Merchant as DM
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id (ShortId)
import Kernel.Utils.Common (fromMaybeM)
import Tools.Auth.DashboardUserAuth (DashboardUser, dashboardRequestorEmail, dashboardRequestorName, requestorHasCapability)

-- | Widens RADAR reads from the caller's own tickets to the whole desk. Not
-- bound to any endpoint: @support.radar.read@\/@write@ gate the calls, this
-- only decides what they return.
readAllCapability :: Text
readAllCapability = "support.radar.read_all"

getRadarTicketsList :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Environment.Flow Common.RadarTicketListRes
getRadarTicketsList merchantShortId opCity dashboardUser mbPageSize mbCursor mbPriority mbStageName mbReporterEmail = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsList merchantShortId opCity scope mbPageSize mbCursor mbPriority mbStageName mbReporterEmail

getRadarTicketsSummary :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Environment.Flow Common.RadarSummaryRes
getRadarTicketsSummary merchantShortId opCity dashboardUser = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsSummary merchantShortId opCity scope

getRadarTicketsReporters :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Environment.Flow Common.RadarReportersRes
getRadarTicketsReporters merchantShortId opCity dashboardUser = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsReporters merchantShortId opCity scope

getRadarTicketsTicket :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Environment.Flow Common.RadarTicketDetailRes
getRadarTicketsTicket merchantShortId opCity dashboardUser ticketId = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsTicket merchantShortId opCity scope ticketId

getRadarTicketsTicketConversation :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Environment.Flow Common.RadarConversationRes
getRadarTicketsTicketConversation merchantShortId opCity dashboardUser ticketId = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsTicketConversation merchantShortId opCity scope ticketId

getRadarTicketsTicketAttachment :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Text -> Environment.Flow Common.RadarAttachmentUrlRes
getRadarTicketsTicketAttachment merchantShortId opCity dashboardUser ticketId attachmentId = do
  scope <- ticketScope dashboardUser
  DRadarTickets.getRadarTicketsTicketAttachment merchantShortId opCity scope ticketId attachmentId

postRadarTicketsTicketReply :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Common.RadarReplyReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketReply merchantShortId opCity dashboardUser ticketId Common.RadarReplyReq {..} = do
  (senderEmail, senderName) <- sessionSender dashboardUser
  scope <- ticketScope dashboardUser
  DRadarTickets.postRadarTicketsTicketReply merchantShortId opCity scope ticketId Common.RadarReplyByUserReq {..}

postRadarTicketsCreate :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Common.RadarCreateTicketReq -> Environment.Flow Common.RadarCreateTicketRes
postRadarTicketsCreate merchantShortId opCity dashboardUser Common.RadarCreateTicketReq {..} = do
  (senderEmail, senderName) <- sessionSender dashboardUser
  DRadarTickets.postRadarTicketsCreate merchantShortId opCity Common.RadarCreateTicketByUserReq {..}

postRadarTicketsTicketUpdateStatus :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Common.RadarUpdateStatusReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketUpdateStatus merchantShortId opCity dashboardUser ticketId req = do
  scope <- ticketScope dashboardUser
  DRadarTickets.postRadarTicketsTicketUpdateStatus merchantShortId opCity scope ticketId req

postRadarTicketsTicketCsat :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Common.RadarCsatReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketCsat merchantShortId opCity dashboardUser ticketId req = do
  scope <- ticketScope dashboardUser
  DRadarTickets.postRadarTicketsTicketCsat merchantShortId opCity scope ticketId req

-- | Holders of 'readAllCapability' — by role or by a per-person grant — and
-- SUPER_ADMIN see every ticket; everyone else sees only the tickets they
-- reported, matched on their session email.
ticketScope :: DashboardUser -> Environment.Flow DRadarTickets.TicketScope
ticketScope dashboardUser = do
  seeAll <- requestorHasCapability dashboardUser readAllCapability
  if seeAll
    then pure DRadarTickets.AllTickets
    else DRadarTickets.OwnTickets . fst <$> sessionSender dashboardUser

-- | The operator as the desk shows them: the session person's email (required
-- -- desk writes must be attributable) and display name, falling back to the
-- email when the person has no name.
sessionSender :: DashboardUser -> Environment.Flow (Text, Text)
sessionSender dashboardUser = do
  mbEmail <- dashboardRequestorEmail dashboardUser
  email <- fromMaybeM (InvalidRequest "Your session has no email; this action requires one.") (nonBlank =<< mbEmail)
  pure (email, fromMaybe email (nonBlank (dashboardRequestorName dashboardUser)))
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s
