{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}

module IssueManagement.Storage.Beam.Issue.IGMIssue where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as IC
import IssueManagement.Tools.UtilsTH hiding (label)

data IGMIssueT f = IGMIssueT
  { bookingId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    customerEmail :: B.C f (Maybe Text),
    customerName :: B.C f (Maybe Text),
    riderId :: B.C f (Maybe Text),
    respondingMerchantId :: B.C f (Maybe Text),
    respondentEntityType :: B.C f (Maybe Text),
    transactionId :: B.C f Text,
    domain :: B.C f Text,
    merchantOperatingCityId :: B.C f (Maybe Text),
    customerPhone :: B.C f (Maybe Text),
    id :: B.C f Text,
    issueRaisedByMerchant :: B.C f (Maybe Text),
    issueStatus :: B.C f IC.Status,
    issueType :: B.C f IC.IssueType,
    merchantId :: B.C f (Maybe Text),
    respondentName :: B.C f (Maybe Text),
    respondentEmail :: B.C f (Maybe Text),
    respondentPhone :: B.C f (Maybe Text),
    respondentAction :: B.C f (Maybe Text),
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMIssueT where
  data PrimaryKey IGMIssueT f = IGMIssueId (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = IGMIssueId . id

type IGMIssue = IGMIssueT Identity

$(enableKVPG ''IGMIssueT ['id] [])

$(mkTableInstancesGenericSchema ''IGMIssueT "igm_issue")
