{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LedgerAdjustmentRequest where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Tools.Beam.UtilsTH

data LedgerAdjustmentRequest = LedgerAdjustmentRequest
  { adminCheckerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    adminCheckerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminMakerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    adminMakerName :: Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    approvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    category :: Domain.Types.LedgerAdjustmentRequest.AdjustmentCategory,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    direction :: Domain.Types.LedgerAdjustmentRequest.AdjustmentDirection,
    documentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest,
    ledgerEntryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry),
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    postedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.LedgerAdjustmentRequest.AdjustmentRequestStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data AdjustmentCategory
  = RideRelatedCredit
  | RideRelatedDebit
  | PayoutRelatedCredit
  | PayoutRelatedDebit
  | TdsReimbursementCredit
  | TdsReimbursementDebit
  | IncentiveCredit
  | IncentiveDebit
  | MiscellaneousCredit
  | MiscellaneousDebit
  | TdsDeductionDebit
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AdjustmentDirection = Credit | Debit deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AdjustmentRequestStatus = PENDING_APPROVAL | APPROVED | REJECTED | POSTED | POST_FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AdjustmentCategory))

$(mkHttpInstancesForEnum (''AdjustmentCategory))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AdjustmentDirection))

$(mkHttpInstancesForEnum (''AdjustmentDirection))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AdjustmentRequestStatus))

$(mkHttpInstancesForEnum (''AdjustmentRequestStatus))
