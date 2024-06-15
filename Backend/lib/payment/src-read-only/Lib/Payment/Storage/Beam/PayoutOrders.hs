{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutOrders where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common

data PayoutOrdersT f = PayoutOrdersT
  { accountDetailsType :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.AccountDetailsType),
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    city :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerEmail :: B.C f Kernel.Prelude.Text,
    customerId :: B.C f Kernel.Prelude.Text,
    entityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    entityName :: B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName),
    id :: B.C f Kernel.Prelude.Text,
    lastStatusCheckedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    merchantId :: B.C f Kernel.Prelude.Text,
    mobileNo :: B.C f Kernel.Prelude.Text,
    orderId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutOrdersT where
  data PrimaryKey PayoutOrdersT f = PayoutOrdersId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutOrdersId <$> id <*> orderId

type PayoutOrders = PayoutOrdersT Identity

$(enableKVPG ''PayoutOrdersT ['id, 'orderId] [])

$(mkTableInstancesGenericSchema ''PayoutOrdersT "payout_orders")
