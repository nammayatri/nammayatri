{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutTransactions where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common

data PayoutTransactionsT f = PayoutTransactionsT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fulfillmentMethod :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    gateWayRefId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    payoutOrderId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.Prelude.Text),
    transactionRef :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutTransactionsT where
  data PrimaryKey PayoutTransactionsT f = PayoutTransactionsId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutTransactionsId <$> id <*> transactionRef

type PayoutTransactions = PayoutTransactionsT Identity

$(enableKVPG (''PayoutTransactionsT) [('id), ('transactionRef)] [])

$(mkTableInstancesGenericSchema (''PayoutTransactionsT) "payout_transactions")
