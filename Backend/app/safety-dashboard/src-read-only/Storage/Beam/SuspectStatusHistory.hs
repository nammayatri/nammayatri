{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SuspectStatusHistory where

import qualified Database.Beam as B
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SuspectStatusHistoryT f = SuspectStatusHistoryT
  { adminApproval :: B.C f (Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval),
    approvedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    dl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    firstName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    flaggedBy :: B.C f (Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy]),
    flaggedStatus :: B.C f Domain.Types.Suspect.FlaggedStatus,
    id :: B.C f Kernel.Prelude.Text,
    lastName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantShortId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    statusChangedReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    voterId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SuspectStatusHistoryT where
  data PrimaryKey SuspectStatusHistoryT f = SuspectStatusHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SuspectStatusHistoryId . id

type SuspectStatusHistory = SuspectStatusHistoryT Identity

$(enableKVPG ''SuspectStatusHistoryT ['id] [])

$(mkTableInstances ''SuspectStatusHistoryT "suspect_status_history")
