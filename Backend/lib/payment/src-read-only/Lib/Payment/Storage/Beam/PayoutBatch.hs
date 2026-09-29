{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutBatch where

import qualified Data.Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch

data PayoutBatchT f = PayoutBatchT
  { clientRefNo :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    excludedCount :: (B.C f Kernel.Prelude.Int),
    executionDate :: (B.C f Data.Time.Day),
    failureCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    itemCount :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    nextStatusCallAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    origin :: (B.C f Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin),
    partnerBatchRef :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    payoutRail :: (B.C f Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail),
    payoutServiceName :: (B.C f Kernel.Prelude.Text),
    resolvedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus),
    statusCheckCalls :: (B.C f Kernel.Prelude.Int),
    statusCheckRound :: (B.C f Kernel.Prelude.Int),
    statusNoDataReplies :: (B.C f Kernel.Prelude.Int),
    submittedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    totalAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutBatchT where
  data PrimaryKey PayoutBatchT f = PayoutBatchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutBatchId . id

type PayoutBatch = PayoutBatchT Identity

$(enableKVPG (''PayoutBatchT) [('id)] [])

$(mkTableInstancesGenericSchema (''PayoutBatchT) "payout_batch")
