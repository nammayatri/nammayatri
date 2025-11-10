{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PaymentOrderSplit where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder

data PaymentOrderSplit = PaymentOrderSplit
  { amount :: Kernel.Types.Common.Price,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit,
    mdrBorneBy :: Kernel.External.Payment.Interface.Types.MBY,
    merchantCommission :: Kernel.Types.Common.Price,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vendorId :: Kernel.Prelude.Text
  }
  deriving (Generic)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''Kernel.External.Payment.Interface.Types.MBY)
