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
  { clientRefNo :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    failureReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    inquiryAttemptsToday :: B.C f Kernel.Prelude.Int,
    inquiryQuotaDate :: B.C f (Kernel.Prelude.Maybe Data.Time.Day),
    itemCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    nextInquiryAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    origin :: B.C f Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin,
    partnerBatchRef :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerResponseCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutRail :: B.C f Kernel.Prelude.Text,
    pendingCount :: B.C f Kernel.Prelude.Int,
    processedCount :: B.C f Kernel.Prelude.Int,
    rejectedCount :: B.C f Kernel.Prelude.Int,
    resolvedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    retryOfBatchId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    runId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus,
    submittedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    totalAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    valueDate :: B.C f Data.Time.Day
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutBatchT where
  data PrimaryKey PayoutBatchT f = PayoutBatchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutBatchId . id

type PayoutBatch = PayoutBatchT Identity

$(enableKVPG ''PayoutBatchT ['id] [['clientRefNo], ['runId]])

$(mkTableInstancesGenericSchema ''PayoutBatchT "payout_batch")
