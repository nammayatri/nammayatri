{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.FinanceAccountConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Account
import Tools.Beam.UtilsTH

data FinanceAccountConfigT f = FinanceAccountConfigT
  { accountType :: (B.C f Lib.Finance.Domain.Types.Account.AccountType),
    allowNegativeBalance :: (B.C f Kernel.Prelude.Bool),
    autoTransferOut :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    counterpartyType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    instantTransferOutEnabled :: (B.C f Kernel.Prelude.Bool),
    maxTransferOutPerDay :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    minRetainedBalance :: (B.C f Kernel.Types.Common.HighPrecMoney),
    minTransferOutAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    redeemableCutoff :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    subLedger :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    transferInEnabled :: (B.C f Kernel.Prelude.Bool),
    transferOutEnabled :: (B.C f Kernel.Prelude.Bool),
    transferOutFee :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FinanceAccountConfigT where
  data PrimaryKey FinanceAccountConfigT f = FinanceAccountConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FinanceAccountConfigId . id

type FinanceAccountConfig = FinanceAccountConfigT Identity

$(enableKVPG (''FinanceAccountConfigT) [('id)] [])

$(mkTableInstancesGenericSchema (''FinanceAccountConfigT) "finance_account_config")
