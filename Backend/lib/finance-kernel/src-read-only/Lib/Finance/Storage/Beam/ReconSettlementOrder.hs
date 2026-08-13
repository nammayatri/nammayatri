{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ReconSettlementOrder where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder
import Tools.Beam.UtilsTH

data ReconSettlementOrderT f = ReconSettlementOrderT
  { allocatedBankCash :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    bffAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    bffType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    claimedGrossAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    claimedSettlementAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    deductionByCollector :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    diffAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    manualConfirmationReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    manuallyConfirmedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    manuallyConfirmedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    messageId :: (B.C f Kernel.Prelude.Text),
    orderId :: (B.C f Kernel.Prelude.Text),
    orderState :: (B.C f Kernel.Prelude.Text),
    orderTransactionId :: (B.C f Kernel.Prelude.Text),
    ourReconStatus :: (B.C f Lib.Finance.Domain.Types.ReconSettlementOrder.OrderReconVerdict),
    paymentStatus :: (B.C f Kernel.Prelude.Text),
    platformGrossFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    platformNetReceivable :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    platformOrderTimestamp :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    rawJson :: (B.C f Kernel.Prelude.Text),
    reasonCode :: (B.C f Kernel.Prelude.Text),
    receivedAt :: (B.C f Kernel.Prelude.UTCTime),
    reconTransactionId :: (B.C f Kernel.Prelude.Text),
    reconciliationStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    remarks :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rideId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementClearedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    settlementDate :: (B.C f Kernel.Prelude.UTCTime),
    settlementId :: (B.C f Kernel.Prelude.Text),
    settlementReferenceNo :: (B.C f Kernel.Prelude.Text),
    settlementType :: (B.C f Kernel.Prelude.Text),
    sourceType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSourceType)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utrSettlementId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    wireOrderReconStatus :: (B.C f Kernel.Prelude.Text),
    wireReconStatus :: (B.C f Kernel.Prelude.Text),
    withholdingTaxGst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    withholdingTaxTds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table ReconSettlementOrderT where
  data PrimaryKey ReconSettlementOrderT f = ReconSettlementOrderId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReconSettlementOrderId . id

type ReconSettlementOrder = ReconSettlementOrderT Identity

$(enableKVPG (''ReconSettlementOrderT) [('id)] [[('messageId)], [('orderId)], [('rideId)], [('utrSettlementId)]])

$(mkTableInstancesGenericSchema (''ReconSettlementOrderT) "recon_settlement_order")
