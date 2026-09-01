{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutRun where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PayoutRun
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PayoutRunT f = PayoutRunT
  { batchCount :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    debitedAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    evaluatedCount :: (B.C f Kernel.Prelude.Int),
    excludedCount :: (B.C f Kernel.Prelude.Int),
    failedAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    failedCount :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    includedCount :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    origin :: (B.C f Domain.Types.PayoutRun.PayoutRunOrigin),
    paidAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    paidCount :: (B.C f Kernel.Prelude.Int),
    parentJobId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    payoutPartner :: (B.C f Kernel.Prelude.Text),
    pendingCount :: (B.C f Kernel.Prelude.Int),
    resolvedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    scheduledFor :: (B.C f Kernel.Prelude.UTCTime),
    sealedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Domain.Types.PayoutRun.PayoutRunStatus),
    totalAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    valueDate :: (B.C f Data.Time.Day)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutRunT where
  data PrimaryKey PayoutRunT f = PayoutRunId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutRunId . id

type PayoutRun = PayoutRunT Identity

$(enableKVPG (''PayoutRunT) [('id)] [[('parentJobId)]])

$(mkTableInstances (''PayoutRunT) "payout_run")
