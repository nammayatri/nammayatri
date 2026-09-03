{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.PgSettlementBatch where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PgSettlementBatchT f = PgSettlementBatchT
  { adjustmentAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    chargebackAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    chargebackReversalAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    charges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    id :: (B.C f Kernel.Prelude.Text),
    mercId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    objectId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    otherAdjustments :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    paymentGateway :: (B.C f Kernel.Prelude.Text),
    payoutAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    payoutMercId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    pvFile :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    pvFileDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    pvNumber :: (B.C f Kernel.Prelude.Text),
    refundAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    refundReversalAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    settlementAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    taxes :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    utrDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))
  }
  deriving (Generic, B.Beamable)

instance B.Table PgSettlementBatchT where
  data PrimaryKey PgSettlementBatchT f = PgSettlementBatchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PgSettlementBatchId . id

type PgSettlementBatch = PgSettlementBatchT Identity

$(enableKVPG (''PgSettlementBatchT) [('id)] [[('pvNumber)], [('utr)]])

$(mkTableInstancesGenericSchema (''PgSettlementBatchT) "pg_settlement_batch")
