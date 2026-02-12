{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.Account where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Account

data AccountT f = AccountT
  { accountCategory :: (B.C f Lib.Finance.Domain.Types.Account.AccountCategory),
    accountType :: (B.C f Lib.Finance.Domain.Types.Account.AccountType),
    balance :: (B.C f Kernel.Types.Common.HighPrecMoney),
    counterpartyId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    counterpartyType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Lib.Finance.Domain.Types.Account.AccountStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AccountT where
  data PrimaryKey AccountT f = AccountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AccountId . id

type Account = AccountT Identity

$(enableKVPG (''AccountT) [('id)] [[('counterpartyId)]])

$(mkTableInstancesGenericSchema (''AccountT) "finance_account")
