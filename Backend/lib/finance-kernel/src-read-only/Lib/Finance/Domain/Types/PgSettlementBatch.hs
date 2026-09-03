{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.PgSettlementBatch where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PgSettlementBatch = PgSettlementBatch
  { adjustmentAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    chargebackAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    chargebackReversalAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    charges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch,
    mercId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    objectId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    otherAdjustments :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paymentGateway :: Kernel.Prelude.Text,
    payoutAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutMercId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pvFile :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pvFileDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pvNumber :: Kernel.Prelude.Text,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundReversalAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    settlementAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    taxes :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    utrDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving (Generic)
