{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Issue where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import qualified IssueManagement.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IssueT f = IssueT
  { bookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    contactEmail :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerId :: B.C f Kernel.Prelude.Text,
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    nightSafety :: B.C f Kernel.Prelude.Bool,
    reason :: B.C f Kernel.Prelude.Text,
    status :: B.C f IssueManagement.Common.IssueStatus,
    ticketId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueT where
  data PrimaryKey IssueT f = IssueId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IssueId . id

type Issue = IssueT Identity

$(enableKVPG ''IssueT ['id] [['bookingId], ['customerId], ['ticketId]])

$(mkTableInstances ''IssueT "issue")

$(Domain.Types.UtilsTH.mkCacParseInstance ''IssueT)
