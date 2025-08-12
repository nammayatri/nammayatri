{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.Refunds where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder

data Refunds = Refunds
  { createdAt :: Kernel.Prelude.UTCTime,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds,
    idAssignedByServiceProvider :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    initiatedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    orderId :: Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    refundAmount :: Kernel.Types.Common.HighPrecMoney,
    shortId :: Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.Refunds.Refunds,
    split :: Kernel.Prelude.Maybe [Lib.Payment.Domain.Types.Refunds.Split],
    status :: Kernel.External.Payment.Interface.RefundStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Split = Split {frfsBookingId :: Kernel.Prelude.Text, splitAmount :: Kernel.Types.Common.HighPrecMoney} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
