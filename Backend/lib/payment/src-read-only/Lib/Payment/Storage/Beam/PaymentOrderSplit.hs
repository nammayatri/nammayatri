{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PaymentOrderSplit where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrderSplit

data PaymentOrderSplitT f = PaymentOrderSplitT
  { currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    mdrBorneBy :: B.C f Kernel.External.Payment.Interface.Types.MBY,
    merchantCommission :: B.C f Kernel.Types.Common.HighPrecMoney,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentOrderId :: B.C f Kernel.Prelude.Text,
    transactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vendorId :: B.C f Kernel.Prelude.Text,
    effectiveAmount :: B.C f Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentOrderSplitT where
  data PrimaryKey PaymentOrderSplitT f = PaymentOrderSplitId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PaymentOrderSplitId . id

type PaymentOrderSplit = PaymentOrderSplitT Identity

$(enableKVPG ''PaymentOrderSplitT ['id] [['paymentOrderId]])

$(mkTableInstancesGenericSchema ''PaymentOrderSplitT "payment_order_split")
