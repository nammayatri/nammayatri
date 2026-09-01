{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutBatchExclusion where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutBatchExclusion

data PayoutBatchExclusionT f = PayoutBatchExclusionT
  { balanceAtEvaluation :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    beneficiaryId :: (B.C f Kernel.Prelude.Text),
    beneficiaryType :: (B.C f Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusionBeneficiaryType),
    correctedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    notifiedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    reason :: (B.C f Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusionReason),
    runId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutBatchExclusionT where
  data PrimaryKey PayoutBatchExclusionT f = PayoutBatchExclusionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutBatchExclusionId . id

type PayoutBatchExclusion = PayoutBatchExclusionT Identity

$(enableKVPG (''PayoutBatchExclusionT) [('id)] [[('beneficiaryId)], [('runId)]])

$(mkTableInstancesGenericSchema (''PayoutBatchExclusionT) "payout_batch_exclusion")
