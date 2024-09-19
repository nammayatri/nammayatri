module IssueManagement.Domain.Action.Beckn.OnIssue where

import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
-- import Kernel.Tools.Metrics.CoreMetrics
-- import Kernel.Types.Error
-- import Kernel.Types.Id
-- import Kernel.Types.TimeRFC339
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

-- import Kernel.Utils.Common

data DOnIssue = DOnIssue
  { id :: Text,
    providerId :: Text,
    providerUrl :: Text,
    respondentName :: Maybe Text,
    respondentEmail :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentAction :: Maybe Text,
    transactionId :: Text,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: Maybe UTCTimeRFC3339
  }

validateRequest ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  DOnIssue ->
  m DIGM.IGMIssue
validateRequest igmIssue = QIGM.findByPrimaryKey (Id igmIssue.id) >>= fromMaybeM (InvalidRequest "Issue not found")

handler ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  DOnIssue ->
  DIGM.IGMIssue ->
  m ()
handler onIssueReq issue = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  let issueStatus = mapActionToStatus onIssueReq.respondentAction & fromMaybe issue.issueStatus
  let updatedIssue =
        issue
          { DIGM.respondentName = onIssueReq.respondentName,
            DIGM.respondentEmail = onIssueReq.respondentEmail,
            DIGM.respondentPhone = onIssueReq.respondentPhone,
            DIGM.respondentAction = onIssueReq.respondentAction,
            DIGM.updatedAt = convertRFC3339ToUTC $ onIssueReq.updatedAt & fromMaybe now,
            DIGM.createdAt = convertRFC3339ToUTC onIssueReq.createdAt,
            DIGM.issueStatus = issueStatus
          }
  QIGM.updateByPrimaryKey updatedIssue
  pure ()

mapActionToStatus :: Maybe Text -> Maybe DIGM.Status
mapActionToStatus (Just "RESOLVED") = Just DIGM.RESOLVED
mapActionToStatus _ = Nothing
