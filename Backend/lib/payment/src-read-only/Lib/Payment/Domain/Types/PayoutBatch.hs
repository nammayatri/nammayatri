{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutBatch where

import Data.Aeson
import qualified Data.Time
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PayoutBatch = PayoutBatch
  { clientRefNo :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch,
    inquiryAttemptsToday :: Kernel.Prelude.Int,
    inquiryQuotaDate :: Kernel.Prelude.Maybe Data.Time.Day,
    itemCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nextInquiryAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    origin :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin,
    partnerBatchRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerResponseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutRail :: Kernel.Prelude.Text,
    pendingCount :: Kernel.Prelude.Int,
    processedCount :: Kernel.Prelude.Int,
    rejectedCount :: Kernel.Prelude.Int,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    retryOfBatchId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch),
    runId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus,
    submittedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    valueDate :: Data.Time.Day
  }
  deriving (Generic)

data PayoutBatchOrigin = SCHEDULED | ADHOC | INSTANT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PayoutBatchStatus
  = OPEN
  | SEALED
  | SUBMITTED
  | SUBMIT_UNKNOWN
  | AWAITING_PARTNER_APPROVAL
  | PARTIALLY_RESOLVED
  | COMPLETED
  | REJECTED
  | AWAITING_BANK
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutBatchOrigin)

$(mkHttpInstancesForEnum ''PayoutBatchOrigin)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutBatchStatus)

$(mkHttpInstancesForEnum ''PayoutBatchStatus)
