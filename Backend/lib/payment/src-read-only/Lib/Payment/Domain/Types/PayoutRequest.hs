{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutRequest where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common

data PayoutRequest = PayoutRequest
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    beneficiaryId :: Kernel.Prelude.Text,
    cashMarkedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cashMarkedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cashMarkedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityName :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName,
    entityRefId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedCreditTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data PayoutRequestStatus
  = INITIATED
  | PROCESSING
  | CREDITED
  | AUTO_PAY_FAILED
  | RETRYING
  | FAILED
  | CANCELLED
  | CASH_PAID
  | CASH_PENDING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutRequestStatus))
