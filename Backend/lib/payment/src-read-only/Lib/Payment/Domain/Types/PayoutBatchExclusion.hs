{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutBatchExclusion where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PayoutBatchExclusion = PayoutBatchExclusion
  { balanceAtEvaluation :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    beneficiaryId :: Kernel.Prelude.Text,
    beneficiaryType :: Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusionBeneficiaryType,
    correctedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion,
    merchantId :: Kernel.Prelude.Text,
    notifiedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reason :: Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusionReason,
    runId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data PayoutBatchExclusionBeneficiaryType = DRIVER | FLEET_OWNER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PayoutBatchExclusionReason
  = BELOW_MINIMUM
  | ZERO_BALANCE
  | PAYOUT_IN_FLIGHT
  | SETTLEMENT_PENDING
  | BANK_DETAILS_MISSING
  | BANK_DETAILS_UNVERIFIED
  | BANK_DETAILS_INVALID
  | ACCOUNT_BLOCKED
  | NOT_REGISTERED_WITH_PARTNER
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutBatchExclusionBeneficiaryType))

$(mkHttpInstancesForEnum (''PayoutBatchExclusionBeneficiaryType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutBatchExclusionReason))

$(mkHttpInstancesForEnum (''PayoutBatchExclusionReason))
