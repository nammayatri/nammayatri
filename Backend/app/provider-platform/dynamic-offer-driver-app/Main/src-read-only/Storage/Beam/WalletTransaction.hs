{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.WalletTransaction where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.WalletTransaction
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data WalletTransactionT f = WalletTransactionT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    paymentOrderId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.WalletTransaction.WalletTransactionStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table WalletTransactionT where
  data PrimaryKey WalletTransactionT f = WalletTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WalletTransactionId . id

type WalletTransaction = WalletTransactionT Identity

$(enableKVPG (''WalletTransactionT) [('id)] [[('driverId)], [('paymentOrderId)]])

$(mkTableInstances (''WalletTransactionT) "wallet_transaction")
