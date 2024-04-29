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
  { bookingId :: (B.C f Data.Text.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    customerEmail :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    customerName :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    customerPhone :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    id :: (B.C f Data.Text.Text),
    issueRaisedByMerchantId :: (B.C f Data.Text.Text),
    issueStatus :: (B.C f Domain.Types.IGMIssue.Status),
    issueType :: (B.C f Domain.Types.IGMIssue.IssueType),
    merchantId :: (B.C f Data.Text.Text),
    resolutionAction :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    respondentAction :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMIssueT where
  data PrimaryKey IGMIssueT f = IGMIssueId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = IGMIssueId . id

type IGMIssue = IGMIssueT Identity

$(enableKVPG (''IGMIssueT) [('id)] [])

$(mkTableInstances (''IGMIssueT) "igm_issue")
