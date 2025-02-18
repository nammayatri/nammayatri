{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutTransaction where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PayoutOrder

data PayoutTransaction = PayoutTransaction
  { amount :: Kernel.Types.Common.Price,
    createdAt :: Kernel.Prelude.UTCTime,
    fulfillmentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gateWayRefId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder,
    status :: Kernel.Prelude.Text,
    transactionRef :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
