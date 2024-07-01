{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IGMIssue where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.IGMIssue
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IGMIssueT f = IGMIssueT
  { bookingId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Data.Text.Text,
    issueStatus :: B.C f Domain.Types.IGMIssue.Status,
    issueType :: B.C f Domain.Types.IGMIssue.IssueType,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    respondentAction :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    respondentEmail :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    respondentEntityType :: B.C f (Kernel.Prelude.Maybe Domain.Types.IGMIssue.Entity),
    respondentName :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    respondentPhone :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    respondingMerchantId :: B.C f Data.Text.Text,
    riderId :: B.C f Data.Text.Text,
    transactionId :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMIssueT where
  data PrimaryKey IGMIssueT f = IGMIssueId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = IGMIssueId . id

type IGMIssue = IGMIssueT Identity

$(enableKVPG ''IGMIssueT ['id] [])

$(mkTableInstances ''IGMIssueT "igm_issue")
