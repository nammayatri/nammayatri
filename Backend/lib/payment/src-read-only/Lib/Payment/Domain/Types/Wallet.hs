{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.Wallet where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account
import qualified Tools.Beam.UtilsTH

data Wallet = Wallet
  { accountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    availableBalance :: Kernel.Types.Common.HighPrecMoney,
    cashbackEarned :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    currentAvailablePoints :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet,
    lifetimeBurned :: Kernel.Types.Common.HighPrecMoney,
    lifetimeEarned :: Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    programId :: Kernel.Prelude.Text,
    programType :: Lib.Finance.Domain.Types.Account.CounterpartyType,
    topupEarned :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
