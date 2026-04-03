{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Finance.Domain.Types.Account where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data Account
    = Account {accountType :: Lib.Finance.Domain.Types.Account.AccountType,
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
               updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
data AccountStatus = Active | Suspended | Closed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data AccountType = Asset | Liability | Revenue | Expense | External | RideCredit deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data CounterpartyType
    = BUYER | SELLER | DRIVER | FLEET_OWNER | GOVERNMENT_DIRECT | GOVERNMENT_INDIRECT | AIRPORT | PG_PAYMENT_JUSPAY | PG_PAYOUT_JUSPAY | RIDER
    deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AccountStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AccountType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CounterpartyType))

