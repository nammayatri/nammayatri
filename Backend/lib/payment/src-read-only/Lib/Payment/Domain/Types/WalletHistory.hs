{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.WalletHistory where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Payment.Domain.Types.Wallet
import qualified Lib.Payment.Domain.Types.WalletPayments
import qualified Tools.Beam.UtilsTH

data WalletHistory = WalletHistory
  { benefitValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    campaignId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    domainEntityId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletHistory.WalletHistory,
    kind :: Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    points :: Kernel.Types.Common.HighPrecMoney,
    programType :: Lib.Finance.Domain.Types.Account.CounterpartyType,
    reversedPoints :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    walletId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet,
    walletPaymentsId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
