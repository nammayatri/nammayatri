{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutOrders where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common

data PayoutOrders = PayoutOrders
  { accountDetailsType :: Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.AccountDetailsType,
    amount :: Kernel.Types.Common.HighPrecMoney,
    city :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerEmail :: Kernel.Prelude.Text,
    customerId :: Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityName :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders,
    lastStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Text,
    mobileNo :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    status :: Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    vpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
