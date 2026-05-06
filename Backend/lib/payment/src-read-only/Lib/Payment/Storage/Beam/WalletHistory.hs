{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.WalletHistory where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Payment.Domain.Types.WalletPayments

data WalletHistoryT f = WalletHistoryT
  { benefitValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    campaignId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    domainEntityId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    kind :: B.C f Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    points :: B.C f Kernel.Types.Common.HighPrecMoney,
    programType :: B.C f Lib.Finance.Domain.Types.Account.CounterpartyType,
    reversedPoints :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    walletId :: B.C f Kernel.Prelude.Text,
    walletPaymentsId :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table WalletHistoryT where
  data PrimaryKey WalletHistoryT f = WalletHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WalletHistoryId . id

type WalletHistory = WalletHistoryT Identity

$(enableKVPG ''WalletHistoryT ['id] [['walletId], ['walletPaymentsId]])

$(mkTableInstancesGenericSchema ''WalletHistoryT "wallet_history")
