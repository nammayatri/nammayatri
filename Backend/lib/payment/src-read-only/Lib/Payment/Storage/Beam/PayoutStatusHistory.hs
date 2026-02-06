{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutStatusHistory where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.PayoutStatusHistory

data PayoutStatusHistoryT f = PayoutStatusHistoryT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    message :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    scheduledPayoutId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Lib.Payment.Domain.Types.PayoutStatusHistory.ScheduledPayoutStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutStatusHistoryT where
  data PrimaryKey PayoutStatusHistoryT f = PayoutStatusHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutStatusHistoryId . id

type PayoutStatusHistory = PayoutStatusHistoryT Identity

$(enableKVPG (''PayoutStatusHistoryT) [('id)] [[('scheduledPayoutId)]])

$(mkTableInstancesGenericSchema (''PayoutStatusHistoryT) "payout_status_history")
