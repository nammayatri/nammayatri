{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Payment.Domain.Types.PaymentOrderSplit where
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.External.Payment.Interface.Types
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data PaymentOrderSplit
    = PaymentOrderSplit {amount :: Kernel.Types.Common.Price,
                         createdAt :: Kernel.Prelude.UTCTime,
                         id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderSplit.PaymentOrderSplit,
                         mdrBorneBy :: Kernel.External.Payment.Interface.Types.MBY,
                         merchantCommission :: Kernel.Types.Common.Price,
                         merchantId :: Kernel.Prelude.Text,
                         merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
                         transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         updatedAt :: Kernel.Prelude.UTCTime,
                         vendorId :: Kernel.Prelude.Text}
    deriving Generic



