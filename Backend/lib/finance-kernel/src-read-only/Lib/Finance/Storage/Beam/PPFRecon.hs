{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.PPFRecon where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.PPFRecon

data PPFReconT f = PPFReconT
  { buyerAppCommissionExpected :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    buyerAppCommissionSettled :: (B.C f Kernel.Types.Common.HighPrecMoney),
    collectorSubscriberId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Prelude.Text),
    domain :: (B.C f Lib.Finance.Domain.Types.PPFRecon.PPFDomain),
    gstAmountExpected :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    gstAmountSettled :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    networkFeeExpected :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    networkFeeSettled :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    networkOrderId :: (B.C f Kernel.Prelude.Text),
    orderAmountExpected :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    orderAmountSettled :: (B.C f Kernel.Types.Common.HighPrecMoney),
    receiverSubscriberId :: (B.C f Kernel.Prelude.Text),
    settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    settlementId :: (B.C f Kernel.Prelude.Text),
    settlementStatus :: (B.C f Lib.Finance.Domain.Types.PPFRecon.PPFSettlementStatus),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table PPFReconT where
  data PrimaryKey PPFReconT f = PPFReconId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PPFReconId . id

type PPFRecon = PPFReconT Identity

$(enableKVPG (''PPFReconT) [('id)] [[('networkOrderId)]])

$(mkTableInstancesGenericSchema (''PPFReconT) "ppf_recon")
