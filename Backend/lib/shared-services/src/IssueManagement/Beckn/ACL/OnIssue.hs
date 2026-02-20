module IssueManagement.Beckn.ACL.OnIssue where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Domain.Action.Beckn.OnIssue as DOnIssue
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnIssueReq ::
  MonadFlow m =>
  Spec.OnIssueReq ->
  m DOnIssue.DOnIssue
buildOnIssueReq req = do
  let context = req.onIssueReqContext
  Utils.validateContext Spec.ON_ISSUE context
  transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  _messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")
  message <- req.onIssueReqMessage & fromMaybeM (InvalidRequest "Message not found")
  let issue = message.onIssueReqMessageIssue
  respondentAction <- fromMaybeM (InvalidRequest "RespondentActions Missing") $ listToMaybe <$> sortBy (comparing (Down . (.respondentActionUpdatedAt))) =<< (.issueActionsRespondentActions) =<< issue.issueIssueActions
  respondentInfo <- respondentAction.respondentActionUpdatedBy & fromMaybeM (InvalidRequest "RespondentActionUpdatedBy Missing")
  let respondentContact = respondentInfo.organizationContact
      respondentPerson = respondentInfo.organizationPerson
  pure $
    DOnIssue.DOnIssue
      { id = issue.issueId,
        providerId = bppSubscriberId,
        providerUrl = bppSubscriberUrl,
        respondentName = respondentPerson >>= (.complainantPersonName),
        respondentEmail = respondentContact >>= (.gROContactEmail),
        respondentPhone = respondentContact >>= (.gROContactPhone),
        transactionId = transactionId,
        respondentAction = respondentAction.respondentActionRespondentAction,
        createdAt = issue.issueCreatedAt,
        updatedAt = respondentAction.respondentActionUpdatedAt
      }
