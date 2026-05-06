{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.WalletPayments where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.WalletPayments

data WalletPaymentsT f = WalletPaymentsT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f Kernel.Types.Common.Currency,
    domainEntityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    orderId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus,
    totalBurned :: B.C f Kernel.Types.Common.HighPrecMoney,
    totalEarned :: B.C f Kernel.Types.Common.HighPrecMoney,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table WalletPaymentsT where
  data PrimaryKey WalletPaymentsT f = WalletPaymentsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WalletPaymentsId . id

type WalletPayments = WalletPaymentsT Identity

$(enableKVPG ''WalletPaymentsT ['id] [['orderId]])

$(mkTableInstancesGenericSchema ''WalletPaymentsT "wallet_payments")
