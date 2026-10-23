{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.FinanceAccountConfig (module Lib.Finance.Domain.Types.FinanceAccountConfig, module ReExport) where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.Extra.FinanceAccountConfig as ReExport
import qualified Lib.Finance.Domain.Types.Extra.FinanceAccountConfig
import qualified Tools.Beam.UtilsTH

data FinanceAccountConfig = FinanceAccountConfig
  { accountType :: Lib.Finance.Domain.Types.Account.AccountType,
    allowNegativeBalance :: Kernel.Prelude.Bool,
    autoTransferOut :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    counterpartyType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig,
    instantTransferOutEnabled :: Kernel.Prelude.Bool,
    maxTransferOutPerDay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    minRetainedBalance :: Kernel.Types.Common.HighPrecMoney,
    minTransferOutAmount :: Kernel.Types.Common.HighPrecMoney,
    redeemableCutoff :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Extra.FinanceAccountConfig.CutoffSpec,
    subLedger :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transferInEnabled :: Kernel.Prelude.Bool,
    transferOutEnabled :: Kernel.Prelude.Bool,
    transferOutFee :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Extra.FinanceAccountConfig.FeeSpec,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
