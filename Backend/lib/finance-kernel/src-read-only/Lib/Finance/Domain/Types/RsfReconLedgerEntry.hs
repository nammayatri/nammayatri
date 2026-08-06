{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.RsfReconLedgerEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RsfReconLedgerEntry = RsfReconLedgerEntry
  { actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfActorType,
    amount :: Kernel.Types.Common.HighPrecMoney,
    bapId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bapUri :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bffAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bffType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    claimStatus :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfClaimStatus,
    collectorAppId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contextTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    counterpartyReconStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Text,
    deductionByCollector :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    diffMessageCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    diffMessageName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    effectiveAt :: Kernel.Prelude.UTCTime,
    entryType :: Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfLedgerEntryType,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry,
    invoiceNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    messageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderPaymentAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    orderState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rawJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rejectionDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    reportedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reportedInMessageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementReasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfLedgerSource,
    ttl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    withholdingTaxGst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    withholdingTaxTds :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RsfActorType = SYSTEM | FINANCE | ADMIN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RsfClaimStatus
  = PENDING
  | ACCEPTED
  | REJECTED_NOT_PAID
  | REJECTED_DETAIL_SUM
  | REJECTED_FARE_MISMATCH
  | REJECTED_BFF_MISMATCH
  | REJECTED_UTR_IMBALANCE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RsfLedgerEntryType
  = MESSAGE_RECEIVED
  | BAP_CLAIM
  | MESSAGE_PROCESSED
  | BANK_RECEIPT
  | BANK_ALLOCATION
  | MANUAL_ADJUSTMENT
  | ORDER_VARIANCE
  | UTR_VARIANCE
  | ORDER_MISSING_IN_BOOK
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RsfLedgerSource = BAP_CLAIMED | BANK_CONFIRMED | FINANCE_MANUAL | SYSTEM_JOB deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RsfActorType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RsfClaimStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RsfLedgerEntryType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RsfLedgerSource))
