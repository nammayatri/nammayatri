{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutStatusHistory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ScheduledPayout
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PayoutStatusHistoryT f = PayoutStatusHistoryT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    scheduledPayoutId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.ScheduledPayout.ScheduledPayoutStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutStatusHistoryT where
  data PrimaryKey PayoutStatusHistoryT f = PayoutStatusHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutStatusHistoryId . id

type PayoutStatusHistory = PayoutStatusHistoryT Identity

$(enableKVPG ''PayoutStatusHistoryT ['id] [['scheduledPayoutId]])

$(mkTableInstances ''PayoutStatusHistoryT "payout_status_history")
