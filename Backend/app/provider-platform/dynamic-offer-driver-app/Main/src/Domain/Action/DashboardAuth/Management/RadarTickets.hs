-- | Hand-written handlers for direct-dashboard RADAR routes whose request names
-- the acting operator.
--
-- The Helper requests (@*ByUserReq@) carry the sender, which provider-dashboard
-- filled from its own session before forwarding the call. The generated
-- @API.Action.DashboardAuth@ handler serves the public request and calls these
-- functions for every endpoint marked @appServerHandler: custom@ in the spec, so
-- the sender always comes from the verified session, never the client.
module Domain.Action.DashboardAuth.Management.RadarTickets
  ( postRadarTicketsTicketReply,
    postRadarTicketsCreate,
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
import Tools.Auth.DashboardUserAuth (DashboardUser, dashboardRequestorEmail, dashboardRequestorName)

postRadarTicketsTicketReply :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Text -> Common.RadarReplyReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketReply merchantShortId opCity dashboardUser ticketId Common.RadarReplyReq {..} = do
  (senderEmail, senderName) <- sessionSender dashboardUser
  DRadarTickets.postRadarTicketsTicketReply merchantShortId opCity ticketId Common.RadarReplyByUserReq {..}

postRadarTicketsCreate :: ShortId DM.Merchant -> Context.City -> DashboardUser -> Common.RadarCreateTicketReq -> Environment.Flow Common.RadarCreateTicketRes
postRadarTicketsCreate merchantShortId opCity dashboardUser Common.RadarCreateTicketReq {..} = do
  (senderEmail, senderName) <- sessionSender dashboardUser
  DRadarTickets.postRadarTicketsCreate merchantShortId opCity Common.RadarCreateTicketByUserReq {..}

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
