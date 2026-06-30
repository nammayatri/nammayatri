module Domain.Action.UI.ZendeskWebhook (postZendeskWebhook) where

import qualified API.Types.UI.ZendeskWebhook as Types
import qualified Data.Text as T
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as IssueCommon
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Sos as DSos
import qualified Safety.Storage.Queries.Sos as SafetyQSos
import Storage.Beam.IssueManagement ()
import Storage.Beam.Sos ()
import Storage.Beam.SystemConfigs ()
import Tools.Error

postZendeskWebhook :: Maybe T.Text -> Types.ZendeskWebhookPayload -> Environment.Flow APISuccess
postZendeskWebhook mbAuth payload = do
  expectedToken <- asks (.zendeskWebhookToken)
  unless (mbAuth == Just ("Bearer " <> expectedToken)) $
    throwError $ AuthBlocked "Invalid Authorization header"
  logInfo $ "ZendeskWebhook: received payload ticketId=" <> show payload.ticket_id <> " status=" <> show payload.status
  case (payload.ticket_id, payload.status) of
    (Just ticketId, Just status) ->
      when (T.toLower status `elem` ["solved", "closed"]) $
        handleTicketClosed ticketId (T.toLower status)
    _ -> logWarning "ZendeskWebhook: missing ticket_id or status in payload"
  return Success

handleTicketClosed :: Text -> Text -> Flow ()
handleTicketClosed ticketId status = do
  mbSos <- SafetyQSos.findByTicketId (Just ticketId)
  case mbSos of
    Just sos -> do
      SafetyQSos.updateStatus DSos.Resolved sos.id
      logInfo $ "ZendeskWebhook: SOS resolved for ticketId=" <> ticketId
    Nothing -> do
      if (status == "solved")
        then do
          QIR.updateIssueStatus ticketId IssueCommon.RESOLVED
          logInfo $ "ZendeskWebhook: IssueReport resolved for ticketId=" <> ticketId
        else do
          QIR.updateIssueStatus ticketId IssueCommon.CLOSED
          logInfo $ "ZendeskWebhook: IssueReport closed for ticketId=" <> ticketId
