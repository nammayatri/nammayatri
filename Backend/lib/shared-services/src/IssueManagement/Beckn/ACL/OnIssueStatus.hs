module IssueManagement.Beckn.ACL.OnIssueStatus where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Domain.Action.Beckn.OnIssueStatus as DOnIssueStatus
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnIssueStatusReq ::
  MonadFlow m =>
  Spec.OnIssueStatusReq ->
  m DOnIssueStatus.DOnIssueStatus
buildOnIssueStatusReq req = do
  let context = req.onIssueStatusReqContext
  Utils.validateContext Spec.ON_ISSUE_STATUS context
  _transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  _messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  _bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")
  message <- req.onIssueStatusReqMessage & fromMaybeM (InvalidRequest "Message not found")
  let issue = message.issueReqMessageIssue
      mbResolution = issue.issueResolution
      gro = issue.issueResolutionProvider >>= (.resolutionProviderRespondentInfo.resolutionProviderRespondentInfoResolutionSupport) >>= (.resolutionSupportGros) >>= listToMaybe
      groContact = gro >>= (.gROContact)
      groPerson = gro >>= (.gROPerson)
  respondentAction <- fromMaybeM (InvalidRequest "RespondentActions Missing") $ listToMaybe <$> sortBy (comparing (Down . (.respondentActionUpdatedAt))) =<< (.issueActionsRespondentActions) =<< issue.issueIssueActions
  pure $
    DOnIssueStatus.DOnIssueStatus
      { id = issue.issueId,
        providerId = bppSubscriberId,
        respondentName = groPerson >>= (.complainantPersonName),
        respondentEmail = groContact >>= (.gROContactEmail),
        respondentPhone = groContact >>= (.gROContactPhone),
        respondentAction = respondentAction.respondentActionRespondentAction,
        resolutionActionTriggered = mbResolution <&> (.issueResolutionActionTriggered),
        updatedAt = issue.issueUpdatedAt
      }
