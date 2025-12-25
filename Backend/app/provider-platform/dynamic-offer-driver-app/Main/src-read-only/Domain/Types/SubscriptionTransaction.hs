{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SubscriptionTransaction where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SubscriptionTransaction = SubscriptionTransaction
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Data.Time.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Driver,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Driver),
    fromLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Location.Location),
    id :: Kernel.Types.Id.Id Domain.Types.SubscriptionTransaction.SubscriptionTransaction,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    status :: Kernel.External.Payment.Juspay.Types.Common.TransactionStatus,
    toLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Location.Location),
    transactionType :: Domain.Types.SubscriptionTransaction.TransactionType,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show)

data TransactionType = RIDE | PLAN_PURCHASE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TransactionType)

$(mkHttpInstancesForEnum ''TransactionType)
