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
    excludedCount :: Kernel.Prelude.Int,
    executionDate :: Data.Time.Day,
    failureCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch,
    itemCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    nextStatusCallAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    origin :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin,
    partnerBatchRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutRail :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail,
    payoutServiceName :: Kernel.Prelude.Text,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus,
    statusCheckCalls :: Kernel.Prelude.Int,
    statusCheckRound :: Kernel.Prelude.Int,
    statusNoDataReplies :: Kernel.Prelude.Int,
    submittedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data PayoutBatchOrigin = SCHEDULED | ADHOC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PayoutBatchRail = NEFT | RTGS | IMPS | A2A deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PayoutBatchStatus
  = CREATED
  | SUBMITTED
  | SUBMIT_UNKNOWN
  | AWAITING_PARTNER_APPROVAL
  | COMPLETED
  | SUBMIT_FAILED
  | MANUAL_REVIEW_REQUIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutBatchOrigin))

$(mkHttpInstancesForEnum (''PayoutBatchOrigin))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutBatchRail))

$(mkHttpInstancesForEnum (''PayoutBatchRail))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutBatchStatus))

$(mkHttpInstancesForEnum (''PayoutBatchStatus))
