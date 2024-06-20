{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Issue where

import qualified Database.Beam as B
import qualified IssueManagement.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IssueT f = IssueT
  { id :: B.C f Kernel.Prelude.Text,
    customerId :: B.C f Kernel.Prelude.Text,
    bookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    contactEmail :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    reason :: B.C f Kernel.Prelude.Text,
    description :: B.C f Kernel.Prelude.Text,
    ticketId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f IssueManagement.Common.IssueStatus,
    nightSafety :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueT where
  data PrimaryKey IssueT f = IssueId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IssueId . id

type Issue = IssueT Identity

$(enableKVPG ''IssueT ['id] [['customerId], ['bookingId], ['ticketId]])

$(mkTableInstances ''IssueT "issue")
