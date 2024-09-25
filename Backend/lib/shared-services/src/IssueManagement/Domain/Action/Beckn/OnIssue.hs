module IssueManagement.Domain.Action.Beckn.OnIssue where

import Data.Aeson
import IGM.Enums
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

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
  let issueStatus = fromMaybe issue.issueStatus $ do
        respondentAction <- onIssueReq.respondentAction
        respondentAction' <- decode (encode respondentAction)
        mapActionToStatus respondentAction'
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

mapActionToStatus :: Maybe ResolutionAction -> Maybe DIGM.Status
mapActionToStatus (Just RESOLVE) = Just DIGM.RESOLVED
mapActionToStatus (Just REJECT) = Just DIGM.CLOSED
mapActionToStatus _ = Nothing
