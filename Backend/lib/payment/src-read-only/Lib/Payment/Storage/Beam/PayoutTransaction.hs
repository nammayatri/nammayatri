{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutTransaction where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common

data PayoutTransactionT f = PayoutTransactionT
  { currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    fulfillmentMethod :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gateWayRefId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutOrderId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Kernel.Prelude.Text,
    transactionRef :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutTransactionT where
  data PrimaryKey PayoutTransactionT f = PayoutTransactionId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutTransactionId <$> id <*> transactionRef

type PayoutTransaction = PayoutTransactionT Identity

$(enableKVPG ''PayoutTransactionT ['id, 'transactionRef] [])

$(mkTableInstancesGenericSchema ''PayoutTransactionT "payout_transaction")
