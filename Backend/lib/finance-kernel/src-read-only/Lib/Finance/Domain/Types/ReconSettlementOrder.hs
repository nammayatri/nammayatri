{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconSettlementOrder where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import qualified Tools.Beam.UtilsTH

data ReconSettlementOrder = ReconSettlementOrder
  { allocatedBankCash :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bffAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bffType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    claimedGrossAmount :: Kernel.Types.Common.HighPrecMoney,
    claimedSettlementAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    deductionByCollector :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    diffAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder,
    invoiceNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    manualConfirmationReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    manuallyConfirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    manuallyConfirmedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    messageId :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    orderState :: Kernel.Prelude.Text,
    orderTransactionId :: Kernel.Prelude.Text,
    ourReconStatus :: Lib.Finance.Domain.Types.ReconSettlementOrder.OrderReconVerdict,
    paymentStatus :: Kernel.Prelude.Text,
    platformGrossFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformNetReceivable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformOrderTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rawJson :: Kernel.Prelude.Text,
    reasonCode :: Kernel.Prelude.Text,
    receivedAt :: Kernel.Prelude.UTCTime,
    reconTransactionId :: Kernel.Prelude.Text,
    reconciliationStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementClearedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementDate :: Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Text,
    settlementReferenceNo :: Kernel.Prelude.Text,
    settlementType :: Kernel.Prelude.Text,
    sourceType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSourceType,
    updatedAt :: Kernel.Prelude.UTCTime,
    utrSettlementId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement),
    wireOrderReconStatus :: Kernel.Prelude.Text,
    wireReconStatus :: Kernel.Prelude.Text,
    withholdingTaxGst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    withholdingTaxTds :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OrderReconVerdict = PENDING | PAID | UNDERPAID | OVERPAID | NOT_PAID | UNMATCHED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconSourceType = BAP_CLAIMED | UNSOLICITED | ADMIN_SETTLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OrderReconVerdict))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ReconSourceType))
