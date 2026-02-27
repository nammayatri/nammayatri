{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconciliationEntry where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.ReconciliationSummary

data ReconciliationEntry = ReconciliationEntry
  { actualLedgerValue :: Kernel.Types.Common.HighPrecMoney,
    bookingId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dcoId :: Kernel.Prelude.Text,
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
    sourceDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.ReconciliationEntry.RideStatus,
    summaryId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary,
    targetDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timestamp :: Kernel.Prelude.UTCTime,
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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationStatus = MATCHED | HIGHER_IN_TARGET | LOWER_IN_TARGET | MISSING_IN_TARGET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationType = DSR_VS_LEDGER | DSR_VS_SUBSCRIPTION | DSSR_VS_SUBSCRIPTION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideMode = ONLINE | CASH deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideStatus = COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''FinanceComponent))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReconciliationStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReconciliationType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''RideMode))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''RideStatus))
