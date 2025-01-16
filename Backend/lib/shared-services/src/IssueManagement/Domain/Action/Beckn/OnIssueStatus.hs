module IssueManagement.Domain.Action.Beckn.OnIssueStatus where

import Control.Applicative
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

data DOnIssueStatus = DOnIssueStatus
  { id :: Text,
    providerId :: Text,
    respondentName :: Maybe Text,
    respondentEmail :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentAction :: Maybe Text,
    resolutionActionTriggered :: Maybe Text,
    updatedAt :: UTCTimeRFC3339
  }

validateRequest ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  DOnIssueStatus ->
  m DIGM.IGMIssue
validateRequest req = QIGM.findByPrimaryKey (Id $ req.id) >>= fromMaybeM (InvalidRequest "Issue not found")

onIssueStatus :: (BeamFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => DOnIssueStatus -> DIGM.IGMIssue -> m ()
onIssueStatus req issue = do
  let respondentAction = req.respondentAction
  let respondentAction' = (decode . encode) =<< respondentAction
  issueStatus <- case respondentAction' >>= mapActionToStatus of
    Just status -> pure status
    Nothing -> throwError $ InvalidRequest "Invalid RespondentActionStatus"
  let updatedIssue =
        issue
          { DIGM.respondentName = req.respondentName <|> issue.respondentName,
            DIGM.respondentEmail = req.respondentEmail <|> issue.respondentEmail,
            DIGM.respondentPhone = req.respondentPhone <|> issue.respondentPhone,
            DIGM.respondentAction = req.respondentAction <|> issue.respondentAction,
            DIGM.issueStatus = issueStatus,
            DIGM.updatedAt = convertRFC3339ToUTC req.updatedAt
          }
  QIGM.updateByPrimaryKey updatedIssue
  pure ()

mapActionToStatus :: RespondentActions -> Maybe DIGM.Status
mapActionToStatus RESOLVED = Just DIGM.RESOLVED
mapActionToStatus CASCADED = Just DIGM.OPEN
mapActionToStatus NEED_MORE_INFO = Just DIGM.OPEN
mapActionToStatus PROCESSING = Just DIGM.OPEN

-- shrey00 : add rating option when accepting/escalating issue
