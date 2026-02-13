{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Account where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data Account = Account
  { accountType :: Lib.Finance.Domain.Types.Account.AccountType,
    balance :: Kernel.Types.Common.HighPrecMoney,
    counterpartyId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    counterpartyType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.Account.AccountStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data AccountStatus = Active | Suspended | Closed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AccountType = Asset | Liability | Revenue | Expense | External | RideCredit deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CounterpartyType = BUYER | SELLER | DRIVER | FLEET_OWNER | GOVERNMENT_DIRECT | GOVERNMENT_INDIRECT | GOVERNMENT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''AccountType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''CounterpartyType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''AccountStatus))
