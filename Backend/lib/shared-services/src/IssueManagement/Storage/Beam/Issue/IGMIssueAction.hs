{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}

module IssueManagement.Storage.Beam.Issue.IGMIssueAction where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified IssueManagement.Domain.Types.Issue.IGMIssueAction as Domain
import IssueManagement.Tools.UtilsTH

data IGMIssueActionT f = IGMIssueActionT
  { id :: B.C f Text,
    igmIssueId :: B.C f Text,
    actionType :: B.C f Domain.ActionType,
    action :: B.C f Text,
    shortDesc :: B.C f (Maybe Text),
    updatedAt :: B.C f UTCTime,
    updatedByOrgName :: B.C f (Maybe Text),
    updatedByContactPhone :: B.C f (Maybe Text),
    updatedByContactEmail :: B.C f (Maybe Text),
    updatedByPersonName :: B.C f (Maybe Text),
    cascadedLevel :: B.C f (Maybe Int),
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMIssueActionT where
  data PrimaryKey IGMIssueActionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IGMIssueAction = IGMIssueActionT Identity

$(enableKVPG ''IGMIssueActionT ['id] [['igmIssueId]])

$(mkTableInstancesGenericSchema ''IGMIssueActionT "igm_issue_action")
