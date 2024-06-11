{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutTransactions where

import qualified Domain.Types.Merchant
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PayoutOrders

data PayoutTransactions = PayoutTransactions
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    fulfillmentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gateWayRefId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions,
    payoutOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders,
    status :: Kernel.Prelude.Text,
    transactionRef :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
