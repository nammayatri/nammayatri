{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutRun where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PayoutRun = PayoutRun
  { batchCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    debitedAmount :: Kernel.Types.Common.HighPrecMoney,
    evaluatedCount :: Kernel.Prelude.Int,
    excludedCount :: Kernel.Prelude.Int,
    failedAmount :: Kernel.Types.Common.HighPrecMoney,
    failedCount :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun,
    includedCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    origin :: Domain.Types.PayoutRun.PayoutRunOrigin,
    paidAmount :: Kernel.Types.Common.HighPrecMoney,
    paidCount :: Kernel.Prelude.Int,
    parentJobId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutPartner :: Kernel.Prelude.Text,
    pendingCount :: Kernel.Prelude.Int,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    scheduledFor :: Kernel.Prelude.UTCTime,
    sealedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.PayoutRun.PayoutRunStatus,
    totalAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    valueDate :: Data.Time.Day
  }
  deriving (Generic)

data PayoutRunOrigin = SCHEDULED | ADHOC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PayoutRunStatus = ASSEMBLING | SEALED | IN_PROGRESS | COMPLETED | PARTIALLY_RESOLVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutRunOrigin))

$(mkHttpInstancesForEnum (''PayoutRunOrigin))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutRunStatus))

$(mkHttpInstancesForEnum (''PayoutRunStatus))
