{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.Wallet where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Account

data WalletT f = WalletT
  { accountId :: (B.C f Kernel.Prelude.Text),
    availableBalance :: (B.C f Kernel.Types.Common.HighPrecMoney),
    cashbackEarned :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    currentAvailablePoints :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    lifetimeBurned :: (B.C f Kernel.Types.Common.HighPrecMoney),
    lifetimeEarned :: (B.C f Kernel.Types.Common.HighPrecMoney),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    personId :: (B.C f Kernel.Prelude.Text),
    programId :: (B.C f Kernel.Prelude.Text),
    programType :: (B.C f Lib.Finance.Domain.Types.Account.CounterpartyType),
    topupEarned :: (B.C f Kernel.Types.Common.HighPrecMoney),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table WalletT where
  data PrimaryKey WalletT f = WalletId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WalletId . id

type Wallet = WalletT Identity

$(enableKVPG (''WalletT) [('id)] [[('accountId)], [('personId)]])

$(mkTableInstancesGenericSchema (''WalletT) "wallet")
