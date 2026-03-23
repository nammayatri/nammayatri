{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconciliationEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Tools.Beam.UtilsTH

data ReconciliationEntry = ReconciliationEntry
  { actualLedgerValue :: Kernel.Types.Common.HighPrecMoney,
    bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dcoId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedDsrValue :: Kernel.Types.Common.HighPrecMoney,
    financeComponent :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.FinanceComponent,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mismatchReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mode :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.RideMode,
    reconStatus :: Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationStatus,
    reconciliationDate :: Kernel.Prelude.UTCTime,
    reconciliationType :: Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationType,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.RideStatus,
    summaryId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary,
    targetDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    targetId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timestamp :: Kernel.Prelude.UTCTime,
    transactionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    variance :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)

data FinanceComponent
  = GROSS_RIDE_FARE
  | GST
  | USER_CANCELLATION
  | DRIVER_CANCELLATION
  | DRIVER_TAKE_HOME_EARNINGS
  | SUBSCRIPTION_PURCHASE
  | EXPIRY
  | PG_PAYMENT_SETTLEMENT
  | PG_PAYOUT_SETTLEMENT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationStatus = MATCHED | HIGHER_IN_TARGET | LOWER_IN_TARGET | MISSING_IN_TARGET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationType
  = DSR_VS_LEDGER
  | DSR_VS_SUBSCRIPTION
  | DSSR_VS_SUBSCRIPTION
  | PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
  | PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideMode = ONLINE | CASH deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideStatus = COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FinanceComponent)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReconciliationStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReconciliationType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RideMode)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RideStatus)
